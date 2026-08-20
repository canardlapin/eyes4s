/*
 * Copyright 2026 canardlapin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * Independent EDF oracle extractor.
 *
 * This file is compiled by a licensed EyeLink SDK installation against the
 * vendor-provided edf.h / edf_data.h and libedfapi. No proprietary header,
 * library, or source file is copied into eyes4s. The output is an atomic fact
 * manifest consumed by the cross-platform oracle court.
 */

#include <ctype.h>
#include <errno.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <edf.h>

typedef struct {
  const char *input;
  const char *output;
  const char *fixture_id;
  const char *oracle_id;
  const char *tool_digest;
  const char *adapter_digest;
  const char *input_digest;
  int consistency;
} options_t;

typedef struct {
  FILE *output;
  unsigned long record;
  unsigned long field;
  unsigned long source_order;
  int block;
  const char *kind;
} writer_t;

static void usage(const char *program) {
  fprintf(stderr,
          "usage: %s --input FILE --output FILE --fixture-id ID --oracle-id ID "
          "--tool-digest SHA256 --adapter-digest SHA256 --input-digest SHA256 "
          "[--consistency 0|1|2]\n",
          program);
}

static int is_id(const char *value) {
  size_t index;
  if (value == NULL || value[0] == '\0' ||
      (!islower((unsigned char)value[0]) && !isdigit((unsigned char)value[0])))
    return 0;
  for (index = 0; value[index] != '\0'; ++index) {
    const unsigned char current = (unsigned char)value[index];
    if (!islower(current) && !isdigit(current) && current != '-') return 0;
  }
  return 1;
}

static int is_sha256(const char *value) {
  size_t index;
  if (value == NULL || strlen(value) != 64) return 0;
  for (index = 0; index < 64; ++index) {
    if (!isdigit((unsigned char)value[index]) &&
        !(value[index] >= 'a' && value[index] <= 'f'))
      return 0;
  }
  return 1;
}

static const char *argument_value(int argc, char **argv, int *index) {
  if (*index + 1 >= argc) return NULL;
  *index += 1;
  return argv[*index];
}

static int parse_options(int argc, char **argv, options_t *options) {
  int index;
  memset(options, 0, sizeof(*options));
  options->consistency = 1;
  for (index = 1; index < argc; ++index) {
    const char *name = argv[index];
    const char *value = NULL;
    if (strcmp(name, "--input") == 0)
      options->input = argument_value(argc, argv, &index);
    else if (strcmp(name, "--output") == 0)
      options->output = argument_value(argc, argv, &index);
    else if (strcmp(name, "--fixture-id") == 0)
      options->fixture_id = argument_value(argc, argv, &index);
    else if (strcmp(name, "--oracle-id") == 0)
      options->oracle_id = argument_value(argc, argv, &index);
    else if (strcmp(name, "--tool-digest") == 0)
      options->tool_digest = argument_value(argc, argv, &index);
    else if (strcmp(name, "--adapter-digest") == 0)
      options->adapter_digest = argument_value(argc, argv, &index);
    else if (strcmp(name, "--input-digest") == 0)
      options->input_digest = argument_value(argc, argv, &index);
    else if (strcmp(name, "--consistency") == 0) {
      value = argument_value(argc, argv, &index);
      if (value == NULL || strlen(value) != 1 || value[0] < '0' || value[0] > '2')
        return 0;
      options->consistency = value[0] - '0';
    } else {
      fprintf(stderr, "unknown argument: %s\n", name);
      return 0;
    }
    if ((strcmp(name, "--consistency") != 0) &&
        ((index >= argc) || argv[index] == NULL))
      return 0;
  }
  return options->input != NULL && options->output != NULL &&
         is_id(options->fixture_id) && is_id(options->oracle_id) &&
         is_sha256(options->tool_digest) && is_sha256(options->adapter_digest) &&
         is_sha256(options->input_digest);
}

static void write_encoded_n(FILE *output, const unsigned char *value, size_t length) {
  size_t index;
  for (index = 0; index < length; ++index) {
    switch (value[index]) {
      case '%': fputs("%25", output); break;
      case '\t': fputs("%09", output); break;
      case '\n': fputs("%0A", output); break;
      case '\r': fputs("%0D", output); break;
      default: fputc(value[index], output); break;
    }
  }
}

static void write_encoded(FILE *output, const char *value) {
  write_encoded_n(output, (const unsigned char *)value, strlen(value));
}

static int is_safe_text(const unsigned char *value, size_t length) {
  size_t index;
  for (index = 0; index < length; ++index) {
    if (value[index] == 0 || (value[index] < 32 && value[index] != '\t' &&
                             value[index] != '\n' && value[index] != '\r'))
      return 0;
  }
  return 1;
}

static void write_hex(FILE *output, const unsigned char *value, size_t length) {
  static const char digits[] = "0123456789abcdef";
  size_t index;
  for (index = 0; index < length; ++index) {
    fputc(digits[value[index] >> 4], output);
    fputc(digits[value[index] & 0x0f], output);
  }
}

static void begin_record(writer_t *writer, const char *kind, int block,
                         unsigned long source_order) {
  writer->record += 1;
  writer->field = 0;
  writer->kind = kind;
  writer->block = block;
  writer->source_order = source_order;
}

static void begin_fact(writer_t *writer, const char *path, const char *presence) {
  writer->field += 1;
  fprintf(writer->output, "%lu\t%lu\t%s\t", writer->record, writer->field,
          writer->kind);
  if (writer->block > 0) fprintf(writer->output, "%d", writer->block);
  fprintf(writer->output, "\t%lu\t%s\t%s\t", writer->source_order, path, presence);
}

static void value_bytes(writer_t *writer, const char *path, const unsigned char *value,
                        size_t length) {
  begin_fact(writer, path, "value");
  write_encoded_n(writer->output, value, length);
  fputs("\t\n", writer->output);
}

static void value_hex(writer_t *writer, const char *path, const unsigned char *value,
                      size_t length) {
  begin_fact(writer, path, "value");
  write_hex(writer->output, value, length);
  fputs("\t\n", writer->output);
}

static void value_ulong(writer_t *writer, const char *path, unsigned long value) {
  begin_fact(writer, path, "value");
  fprintf(writer->output, "%lu\t\n", value);
}

static void value_long(writer_t *writer, const char *path, long value) {
  begin_fact(writer, path, "value");
  fprintf(writer->output, "%ld\t\n", value);
}

static void value_float(writer_t *writer, const char *path, float value) {
  if (isfinite(value)) {
    begin_fact(writer, path, "value");
    fprintf(writer->output, "%.9g\t\n", (double)value);
  } else {
    begin_fact(writer, path, "missing");
    fputs("\tedf-access-api-nonfinite\n", writer->output);
  }
}

static void omitted(writer_t *writer, const char *path, const char *detail) {
  begin_fact(writer, path, "omitted");
  fputc('\t', writer->output);
  write_encoded(writer->output, detail);
  fputc('\n', writer->output);
}

static void sample_facts(writer_t *writer, const FSAMPLE *sample) {
  int eye;
  int item;
  char path[80];
  value_ulong(writer, "sample.time", (unsigned long)sample->time);
  value_ulong(writer, "sample.flags", (unsigned long)sample->flags);
  for (eye = 0; eye < 2; ++eye) {
    const char *side = eye == 0 ? "left" : "right";
#define EYES4S_EYE_FLOAT(name)                                                   \
  snprintf(path, sizeof(path), "sample.%s.%s", side, #name);                    \
  value_float(writer, path, sample->name[eye])
    EYES4S_EYE_FLOAT(px);
    EYES4S_EYE_FLOAT(py);
    EYES4S_EYE_FLOAT(hx);
    EYES4S_EYE_FLOAT(hy);
    EYES4S_EYE_FLOAT(pa);
    EYES4S_EYE_FLOAT(gx);
    EYES4S_EYE_FLOAT(gy);
    EYES4S_EYE_FLOAT(gxvel);
    EYES4S_EYE_FLOAT(gyvel);
    EYES4S_EYE_FLOAT(hxvel);
    EYES4S_EYE_FLOAT(hyvel);
    EYES4S_EYE_FLOAT(rxvel);
    EYES4S_EYE_FLOAT(ryvel);
    EYES4S_EYE_FLOAT(fgxvel);
    EYES4S_EYE_FLOAT(fgyvel);
    EYES4S_EYE_FLOAT(fhxvel);
    EYES4S_EYE_FLOAT(fhyvel);
    EYES4S_EYE_FLOAT(frxvel);
    EYES4S_EYE_FLOAT(fryvel);
#undef EYES4S_EYE_FLOAT
  }
  value_float(writer, "sample.rx", sample->rx);
  value_float(writer, "sample.ry", sample->ry);
  value_ulong(writer, "sample.status", (unsigned long)sample->status);
  value_ulong(writer, "sample.input", (unsigned long)sample->input);
  value_ulong(writer, "sample.buttons", (unsigned long)sample->buttons);
  value_long(writer, "sample.htype", (long)sample->htype);
  for (item = 0; item < 8; ++item) {
    snprintf(path, sizeof(path), "sample.hdata.%d", item);
    value_long(writer, path, (long)sample->hdata[item]);
  }
  value_ulong(writer, "sample.errors", (unsigned long)sample->errors);
}

static void event_facts(writer_t *writer, const FEVENT *event) {
  value_ulong(writer, "event.effective-time", (unsigned long)event->time);
  value_long(writer, "event.type", (long)event->type);
  value_ulong(writer, "event.read-flags", (unsigned long)event->read);
  value_long(writer, "event.eye", (long)event->eye);
  value_ulong(writer, "event.start-time", (unsigned long)event->sttime);
  value_ulong(writer, "event.end-time", (unsigned long)event->entime);
#define EYES4S_EVENT_FLOAT(name) value_float(writer, "event." #name, event->name)
  EYES4S_EVENT_FLOAT(hstx);
  EYES4S_EVENT_FLOAT(hsty);
  EYES4S_EVENT_FLOAT(gstx);
  EYES4S_EVENT_FLOAT(gsty);
  EYES4S_EVENT_FLOAT(sta);
  EYES4S_EVENT_FLOAT(henx);
  EYES4S_EVENT_FLOAT(heny);
  EYES4S_EVENT_FLOAT(genx);
  EYES4S_EVENT_FLOAT(geny);
  EYES4S_EVENT_FLOAT(ena);
  EYES4S_EVENT_FLOAT(havx);
  EYES4S_EVENT_FLOAT(havy);
  EYES4S_EVENT_FLOAT(gavx);
  EYES4S_EVENT_FLOAT(gavy);
  EYES4S_EVENT_FLOAT(ava);
  EYES4S_EVENT_FLOAT(avel);
  EYES4S_EVENT_FLOAT(pvel);
  EYES4S_EVENT_FLOAT(svel);
  EYES4S_EVENT_FLOAT(evel);
  EYES4S_EVENT_FLOAT(supd_x);
  EYES4S_EVENT_FLOAT(eupd_x);
  EYES4S_EVENT_FLOAT(supd_y);
  EYES4S_EVENT_FLOAT(eupd_y);
#undef EYES4S_EVENT_FLOAT
  value_ulong(writer, "event.status", (unsigned long)event->status);
  value_ulong(writer, "event.flags", (unsigned long)event->flags);
  value_ulong(writer, "event.input", (unsigned long)event->input);
  value_ulong(writer, "event.buttons", (unsigned long)event->buttons);
  value_ulong(writer, "event.parsed-by", (unsigned long)event->parsedby);
}

static void message_facts(writer_t *writer, const FEVENT *event) {
  const LSTRING *message = event->message;
  size_t length = message == NULL || message->len < 0 ? 0 : (size_t)message->len;
  const unsigned char *bytes =
      message == NULL ? (const unsigned char *)"" : (const unsigned char *)&message->c;
  value_ulong(writer, "message.effective-time", (unsigned long)event->time);
  omitted(writer, "message.raw-time",
          "edf-access-api-fevent-exposes-effective-time-not-raw-logged-time");
  value_ulong(writer, "message.length", (unsigned long)length);
  value_hex(writer, "message.payload-hex", bytes, length);
  if (is_safe_text(bytes, length))
    value_bytes(writer, "message.payload", bytes, length);
  else
    omitted(writer, "message.payload",
            "edf-message-contains-control-or-nul-bytes-see-payload-hex");
  omitted(writer, "message.offset",
          "edf-access-api-does-not-expose-integration-message-offset-separately");
}

static void recording_facts(writer_t *writer, const RECORDINGS *recording) {
  value_ulong(writer, "recording.time", (unsigned long)recording->time);
  value_long(writer, "recording.state", (long)recording->state);
  value_long(writer, "recording.record-type", (long)recording->record_type);
  value_long(writer, "recording.pupil-type", (long)recording->pupil_type);
  value_long(writer, "recording.mode", (long)recording->recording_mode);
  value_long(writer, "recording.filter-type", (long)recording->filter_type);
  value_float(writer, "recording.sample-rate", recording->sample_rate);
  value_long(writer, "recording.position-type", (long)recording->pos_type);
  value_long(writer, "recording.eye", (long)recording->eye);
  value_ulong(writer, "recording.event-flags", (unsigned long)recording->eflags);
  value_ulong(writer, "recording.sample-flags", (unsigned long)recording->sflags);
}

static const char *event_kind(int type) {
  switch (type) {
    case STARTBLINK: return "native-blink-start";
    case ENDBLINK: return "native-blink-end";
    case STARTSACC: return "native-saccade-start";
    case ENDSACC: return "native-saccade-end";
    case STARTFIX: return "native-fixation-start";
    case ENDFIX: return "native-fixation-end";
    case FIXUPDATE: return "native-fixation-update";
    case STARTSAMPLES: return "sample-stream-start";
    case ENDSAMPLES: return "sample-stream-end";
    case STARTEVENTS: return "event-stream-start";
    case ENDEVENTS: return "event-stream-end";
    case STARTPARSE: return "parse-start";
    case ENDPARSE: return "parse-end";
    case BREAKPARSE: return "parse-break";
    default: return "event";
  }
}

static int is_float_event(int type) {
  switch (type) {
    case STARTBLINK:
    case ENDBLINK:
    case STARTSACC:
    case ENDSACC:
    case STARTFIX:
    case ENDFIX:
    case FIXUPDATE:
    case STARTSAMPLES:
    case ENDSAMPLES:
    case STARTEVENTS:
    case ENDEVENTS:
    case STARTPARSE:
    case ENDPARSE:
    case BREAKPARSE: return 1;
    default: return 0;
  }
}

static int write_manifest(const options_t *options) {
  int error = 0;
  int type;
  int current_block = 0;
  unsigned long source_order = 0;
  EDFFILE *edf = edf_open_file(options->input, options->consistency, 1, 1, &error);
  FILE *output;
  writer_t writer;
  const char *version;
  char invocation[96];

  if (edf == NULL || error != 0) {
    fprintf(stderr, "edf_open_file failed: input=%s error=%d\n", options->input, error);
    if (edf != NULL) edf_close_file(edf);
    return 0;
  }
  output = fopen(options->output, "wb");
  if (output == NULL) {
    fprintf(stderr, "cannot open output=%s: %s\n", options->output, strerror(errno));
    edf_close_file(edf);
    return 0;
  }
  version = edf_get_version();
  if (version == NULL || version[0] == '\0') version = "unreported";
  snprintf(invocation, sizeof(invocation),
           "edf_open_file(consistency=%d,load_events=1,load_samples=1)",
           options->consistency);

  fputs("# eyes4s-eyelink-oracle\t1\t", output);
  write_encoded(output, options->oracle_id);
  fputc('\t', output);
  write_encoded(output, options->fixture_id);
  fputs("\tedf-access-api\tedf\tcomplete\tSR Research EDF Access API\t", output);
  write_encoded(output, version);
  fputc('\t', output);
  fputs(options->tool_digest, output);
  fputc('\t', output);
  fputs(options->adapter_digest, output);
  fputc('\t', output);
  fputs(options->input_digest, output);
  fputc('\t', output);
  write_encoded(output, invocation);
  fputs("\t\n", output);
  fputs("record_ordinal\tfield_ordinal\trecord_kind\tblock\tsource_order\t"
        "field_path\tpresence\tvalue\tdetail\n",
        output);
  memset(&writer, 0, sizeof(writer));
  writer.output = output;

  while ((type = edf_get_next_data(edf)) != NO_PENDING_ITEMS) {
    ALLF_DATA *data;
    source_order += 1;
    data = edf_get_float_data(edf);
    if (data == NULL) {
      begin_record(&writer, "unreadable-element", current_block, source_order);
      value_long(&writer, "element.type-code", (long)type);
      omitted(&writer, "element.data", "edf-get-float-data-returned-null");
      continue;
    }
    if (type == RECORDING_INFO) {
      if (data->rec.state != 0) current_block += 1;
      begin_record(&writer, "recording-metadata", current_block, source_order);
      recording_facts(&writer, &data->rec);
      if (data->rec.state == 0) current_block = 0;
    } else if (type == SAMPLE_TYPE) {
      begin_record(&writer, "sample", current_block, source_order);
      sample_facts(&writer, &data->fs);
    } else if (type == MESSAGEEVENT) {
      begin_record(&writer, "message", current_block, source_order);
      message_facts(&writer, &data->fe);
    } else if (type == BUTTONEVENT || type == INPUTEVENT) {
      begin_record(&writer, type == BUTTONEVENT ? "button" : "input", current_block,
                   source_order);
      value_ulong(&writer, "io.time", (unsigned long)data->io.time);
      value_long(&writer, "io.type", (long)data->io.type);
      value_ulong(&writer, "io.data", (unsigned long)data->io.data);
    } else if (is_float_event(type)) {
      begin_record(&writer, event_kind(type), current_block, source_order);
      event_facts(&writer, &data->fe);
    } else {
      begin_record(&writer, "unknown-element", current_block, source_order);
      value_long(&writer, "element.type-code", (long)type);
      omitted(&writer, "element.data", "unsupported-edf-access-api-element-type");
    }
  }
  if (writer.record == 0) {
    fprintf(stderr, "EDF oracle contains no records: input=%s\n", options->input);
    fclose(output);
    edf_close_file(edf);
    return 0;
  }
  if (fclose(output) != 0) {
    fprintf(stderr, "failed closing output=%s\n", options->output);
    edf_close_file(edf);
    return 0;
  }
  if (edf_close_file(edf) != 0) {
    fprintf(stderr, "edf_close_file failed: input=%s\n", options->input);
    return 0;
  }
  return 1;
}

int main(int argc, char **argv) {
  options_t options;
  if (!parse_options(argc, argv, &options)) {
    usage(argv[0]);
    return 2;
  }
  return write_manifest(&options) ? 0 : 1;
}
