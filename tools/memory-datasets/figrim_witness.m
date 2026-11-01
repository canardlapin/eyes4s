function figrim_witness(source, directory, width, height, cols, rows)
% FIGRIM raw-MAT admission witness.  This deliberately has no Python inputs.
  if nargin < 1 || isempty(source), source = '.datasets/memory/figrim/allImages_release.mat'; end
  if nargin < 2 || isempty(directory), directory = '.'; end
  if nargin < 3 || isempty(width), width = 1000; end
  if nargin < 4 || isempty(height), height = 1000; end
  if nargin < 5 || isempty(cols), cols = 10; end
  if nargin < 6 || isempty(rows), rows = 10; end
  if ~(isscalar(width) && isscalar(height) && isscalar(cols) && isscalar(rows) && ...
       all(isfinite([width height cols rows])) && all([width height cols rows] > 0) && ...
       all([width height cols rows] == fix([width height cols rows])))
    error('figrim_witness:geometry', 'width, height, cols, and rows must be positive integers');
  end
  if exist(directory, 'dir') ~= 7, mkdir(directory); end
  raw = load(source);
  if ~isfield(raw, 'allImages') || ~isstruct(raw.allImages) || ~isvector(raw.allImages)
    error('figrim_witness:shape', 'MAT file must contain vector struct allImages');
  end
  images = raw.allImages(:)';
  nimages = numel(images);
  if nimages == 0, error('figrim_witness:shape', 'allImages must be nonempty'); end
  required = {'filename', 'category', 'userdata'};
  for q = 1:numel(required)
    if ~isfield(images, required{q}), error('figrim_witness:shape', 'allImages lacks %s', required{q}); end
  end
  names = cell(1, nimages); cats = cell(1, nimages);
  for i = 1:nimages
    names{i} = witness_string(images(i).filename, sprintf('image %d filename', i));
    cats{i} = witness_string(images(i).category, sprintf('image %d category', i));
  end
  if numel(unique(names)) ~= nimages, error('figrim_witness:duplicate', 'duplicate filenames'); end
  nslots = numel(images(1).userdata);
  if ~isstruct(images(1).userdata) || nslots == 0
    error('figrim_witness:shape', 'image 1 userdata must be nonempty struct array');
  end
  for i = 1:nimages
    if ~isstruct(images(i).userdata) || numel(images(i).userdata) ~= nslots
      error('figrim_witness:shape', 'userdata slot count differs at image %d', i);
    end
  end
  manifest = witness_manifest(source);
  excluded = manifest.duplicate_image_pairs;
  phases = {'enc', 'rec', 'rec2'};
  phase_file = fullfile(directory, 'phase-ledger.tsv');
  loc_file = fullfile(directory, 'location-ledger.tsv');
  bin_file = fullfile(directory, 'bins.tsv');
  pair_file = fullfile(directory, 'pairs.tsv');
  pf = fopen(phase_file, 'w'); lf = fopen(loc_file, 'w'); bf = fopen(bin_file, 'w'); xf = fopen(pair_file, 'w');
  if any([pf lf bf xf] < 0), error('figrim_witness:output', 'cannot create output TSV'); end
  unwind_protect
    fprintf(pf, 'image_index\timage\tcategory\tobserver_slot\trecording_label\tinitials\tsource_trial\tphase\tslot_state\tgaze_state\tsdt\tresponse_state\tsource_locations\tadmitted_locations\n');
    fprintf(lf, 'image_index\tobserver_slot\tphase\tordinal\traw_x\traw_y\trounded_x\trounded_y\tadmitted\tx_px\ty_px\n');
    fprintf(bf, 'image_index\tobserver_slot\tphase\tbin_x\tbin_y\tcount\n');
    fprintf(xf, 'observer_slot\timage\tphase\tencoding_image\tcontrol_image\n');
    enc_admitted = false(nimages, nslots);
    focal_admitted = false(nimages, nslots, 3);
    for i = 1:nimages
      for j = 1:nslots
        u = images(i).userdata(j);
        [slot_state, recording, initials, trial, sdt_values] = witness_metadata(u, i, j);
        for p = 1:3
          phase = phases{p};
          [sdt, response] = witness_sdt_phase(sdt_values, p);
          [state, locations] = witness_phase(u, phase, i, j);
          source_count = size(locations, 1); admitted_count = 0;
          bincounts = zeros(cols, rows);
          for k = 1:source_count
            x = locations(k, 1); y = locations(k, 2);
            rx = witness_round(x); ry = witness_round(y);
            admitted = isfinite(x) && isfinite(y) && rx >= 1 && rx <= width && ry >= 1 && ry <= height;
            if admitted
              admitted_count = admitted_count + 1;
              bx = floor((rx - 1) * cols / width); by = floor((ry - 1) * rows / height);
              bincounts(bx + 1, by + 1) = bincounts(bx + 1, by + 1) + 1;
              xpx = rx - 1; ypx = ry - 1;
            else
              xpx = []; ypx = [];
            end
            fprintf(lf, '%d\t%d\t%s\t%d\t%s\t%s\t%s\t%s\t%d\t%s\t%s\n', i, j, phase, k, witness_number(x), witness_number(y), witness_number(rx), witness_number(ry), admitted, witness_number_or_blank(xpx), witness_number_or_blank(ypx));
          end
          for bx = 0:cols-1
            for by = 0:rows-1
              c = bincounts(bx + 1, by + 1);
              if c > 0, fprintf(bf, '%d\t%d\t%s\t%d\t%d\t%d\n', i, j, phase, bx, by, c); end
            end
          end
          fprintf(pf, '%d\t%s\t%s\t%d\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%s\t%d\t%d\n', i, names{i}, cats{i}, j, recording, initials, trial, phase, slot_state, state, sdt, response, source_count, admitted_count);
          focal_admitted(i, j, p) = admitted_count > 0;
          if p == 1, enc_admitted(i, j) = admitted_count > 0; end
        end
      end
    end
    for i = 1:nimages
      for j = 1:nslots
        if ~enc_admitted(i, j), continue; end
        for p = 2:3
          if ~focal_admitted(i, j, p), continue; end
          control = witness_control(i, j, names, cats, enc_admitted, excluded);
          if ~isempty(control)
            fprintf(xf, '%d\t%s\t%s\t%s\t%s\n', j, names{i}, phases{p}, names{i}, names{control});
          end
        end
      end
    end
  unwind_protect_cleanup
    fclose(pf); fclose(lf); fclose(bf); fclose(xf);
  end_unwind_protect
end

function [slot_state, recording, initials, trial, sdt_values] = witness_metadata(u, i, j)
  fields = {'trial', 'RECORDING_SESSION_LABEL', 'sinit', 'SDT', 'fixations'};
  for q = 1:numel(fields)
    if ~isfield(u, fields{q}), error('figrim_witness:metadata', 'image %d slot %d lacks %s', i, j, fields{q}); end
  end
  all_fields = fieldnames(u);
  empty = true;
  for q = 1:numel(all_fields)
    empty = empty && isempty(u.(all_fields{q}));
  end
  if empty, slot_state = 'empty_slot'; else slot_state = 'metadata_present'; end
  recording = witness_optional_string(u.RECORDING_SESSION_LABEL, sprintf('image %d slot %d recording label', i, j));
  initials = witness_optional_string(u.sinit, sprintf('image %d slot %d initials', i, j));
  if isempty(u.trial), trial = ''; else
    if ~(isnumeric(u.trial) && isscalar(u.trial) && ((isfinite(u.trial) && u.trial == fix(u.trial)) || isnan(u.trial)))
      error('figrim_witness:trial', 'image %d slot %d trial must be scalar finite integer or NaN', i, j);
    end
    trial = witness_number(u.trial);
  end
  if isempty(u.SDT), sdt_values = []; else
    if ~(isnumeric(u.SDT) && numel(u.SDT) == 3 && all(isfinite(u.SDT(:))) && all(u.SDT(:) == fix(u.SDT(:))))
      error('figrim_witness:sdt', 'image %d slot %d SDT must contain three finite integers', i, j);
    end
    sdt_values = u.SDT(:)';
  end
end

function [sdt, response] = witness_sdt_phase(values, phase_index)
  if isempty(values)
    sdt = ''; response = 'absent'; return;
  end
  code = values(phase_index);
  sdt = witness_number(code);
  if code == -1, response = 'missing_sentinel'; elseif any(code == 1:4), response = 'recorded'; else response = 'unknown_code'; end
end

function [state, locations] = witness_phase(u, phase, i, j)
  locations = zeros(0, 2);
  if isempty(u.fixations), state = 'absent_fixation_struct'; return; end
  if ~isstruct(u.fixations) || numel(u.fixations) ~= 1
    error('figrim_witness:shape', 'image %d slot %d fixations must be scalar struct', i, j);
  end
  if ~isfield(u.fixations, phase), state = 'missing_phase'; return; end
  locations = u.fixations.(phase);
  if isempty(locations), locations = zeros(0, 2); state = 'empty_phase'; return; end
  if ~(isnumeric(locations) && ismatrix(locations) && size(locations, 2) == 2)
    error('figrim_witness:shape', 'image %d slot %d %s must be numeric Nx2', i, j, phase);
  end
  % Integer MAT arrays otherwise keep integer arithmetic through bin division.
  locations = double(locations);
  state = 'locations';
end

function r = witness_round(x)
  if ~isfinite(x), r = x; else r = sign(x) * floor(abs(x) + 0.5); end
end

function s = witness_number(x)
  if isnan(x), s = 'nan'; elseif isinf(x), if x > 0, s = 'inf'; else s = '-inf'; end; else s = sprintf('%.17g', x); end
end
function s = witness_number_or_blank(x), if isempty(x), s = ''; else s = witness_number(x); end; end
function s = witness_string(x, what)
  if isstring(x) && isscalar(x), x = char(x); end
  if ~(ischar(x) && (size(x, 1) == 1)), error('figrim_witness:string', '%s must be a scalar string', what); end
  if any(x == sprintf('\t')) || any(x == sprintf('\n')) || any(x == sprintf('\r')), error('figrim_witness:string', '%s contains TSV control characters', what); end
  s = x;
end
function s = witness_optional_string(x, what), if isempty(x), s = ''; else s = witness_string(x, what); end; end

function manifest = witness_manifest(source)
  here = fileparts(mfilename('fullpath'));
  mf = fullfile(here, 'manifest.json');
  if exist(mf, 'file') ~= 2, error('figrim_witness:manifest', 'manifest.json not found beside witness'); end
  decoded = jsondecode(fileread(mf)); manifest = [];
  for q = 1:numel(decoded.datasets)
    if iscell(decoded.datasets), dataset = decoded.datasets{q}; else dataset = decoded.datasets(q); end
    if strcmp(dataset.id, 'figrim'), manifest = dataset.analysis; break; end
  end
  if isempty(manifest) || ~isfield(manifest, 'duplicate_image_pairs'), error('figrim_witness:manifest', 'figrim duplicate pairs unavailable'); end
end
function control = witness_control(i, j, names, cats, enc_admitted, excluded)
  control = [];
  candidates = find(strcmp(cats, cats{i}) & (1:numel(names)) ~= i & enc_admitted(:, j)');
  [~, order] = sort(names(candidates)); candidates = candidates(order);
  for q = 1:numel(candidates)
    k = candidates(q);
    if ~witness_excluded(names{i}, names{k}, excluded), control = k; return; end
  end
end
function yes = witness_excluded(a, b, pairs)
  yes = false;
  for q = 1:numel(pairs)
    if iscell(pairs), p = pairs{q}; else p = pairs(q, :); end
    if iscell(p), left = p{1}; right = p{2}; else left = p(1, :); right = p(2, :); end
    if (strcmp(a, left) && strcmp(b, right)) || (strcmp(a, right) && strcmp(b, left)), yes = true; return; end
  end
end
