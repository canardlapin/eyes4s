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

package eyes4s.codec

/** Pinned supplied-map recipe; matches repetition-plan-v1.json as JSON. */
object RepetitionPlanFixture:
  // format: off
  val versionOne = """{
  "schema": {
    "name": "example.repetition-plan",
    "version": 1
  },
  "value": {
    "layout": {
      "name": "example.repetition-layout",
      "version": 1
    },
    "keySchema": {
      "name": "example.repetition-key",
      "version": 1
    },
    "projections": [
      {
        "name": "example.person",
        "version": 1
      },
      {
        "name": "example.stimulus",
        "version": 1
      },
      {
        "name": "example.occasion",
        "version": 1
      }
    ],
    "method": "Cosine",
    "methodRevision": 1,
    "orientation": "directed",
    "self": "exclude",
    "matched": [
      "SameParticipant",
      "DifferentOccasion",
      "SameStimulus"
    ],
    "control": [
      "SameParticipant",
      "DifferentOccasion",
      "DifferentStimulus"
    ],
    "selection": {
      "kind": "bottomK",
      "cap": 2,
      "seed": "-9223372036854775801",
      "sampleId": "finite-controls"
    },
    "policy": {
      "kind": "requireAll"
    },
    "frame": {
      "id": "repetition-plan",
      "unit": "px",
      "xMin": 0.0,
      "yMin": 0.0,
      "xMax": 5.0,
      "yMax": 3.0,
      "yAxis": "Down"
    },
    "grid": {
      "id": "repetition-plan@5x3",
      "nx": 5,
      "ny": 3
    },
    "rows": [
      {
        "key": {
          "schema": {
            "name": "example.repetition-key",
            "version": 1
          },
          "value": {
            "person": 1,
            "stimulus": "a",
            "phase": "repeat",
            "repeat": 0
          }
        },
        "values": [
          0.0625,
          0.125,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625
        ],
        "provenance": {
          "inputs": "3a1836652c1b908a",
          "steps": [
            {
              "operation": "normalise",
              "params": [
                {
                  "name": "of",
                  "kind": "text",
                  "value": "surface"
                }
              ]
            }
          ]
        }
      },
      {
        "key": {
          "schema": {
            "name": "example.repetition-key",
            "version": 1
          },
          "value": {
            "person": 1,
            "stimulus": "a",
            "phase": "repeat",
            "repeat": 1
          }
        },
        "values": [
          0.0625,
          0.0625,
          0.125,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625
        ],
        "provenance": {
          "inputs": "6c2d03920877ae2a",
          "steps": [
            {
              "operation": "normalise",
              "params": [
                {
                  "name": "of",
                  "kind": "text",
                  "value": "surface"
                }
              ]
            }
          ]
        }
      },
      {
        "key": {
          "schema": {
            "name": "example.repetition-key",
            "version": 1
          },
          "value": {
            "person": 1,
            "stimulus": "a",
            "phase": "repeat",
            "repeat": 2
          }
        },
        "values": [
          0.0625,
          0.0625,
          0.0625,
          0.125,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625
        ],
        "provenance": {
          "inputs": "c838e1141812528a",
          "steps": [
            {
              "operation": "normalise",
              "params": [
                {
                  "name": "of",
                  "kind": "text",
                  "value": "surface"
                }
              ]
            }
          ]
        }
      },
      {
        "key": {
          "schema": {
            "name": "example.repetition-key",
            "version": 1
          },
          "value": {
            "person": 1,
            "stimulus": "b",
            "phase": "repeat",
            "repeat": 0
          }
        },
        "values": [
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.125,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625
        ],
        "provenance": {
          "inputs": "f755f462f999e8aa",
          "steps": [
            {
              "operation": "normalise",
              "params": [
                {
                  "name": "of",
                  "kind": "text",
                  "value": "surface"
                }
              ]
            }
          ]
        }
      },
      {
        "key": {
          "schema": {
            "name": "example.repetition-key",
            "version": 1
          },
          "value": {
            "person": 1,
            "stimulus": "b",
            "phase": "repeat",
            "repeat": 1
          }
        },
        "values": [
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.125,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625
        ],
        "provenance": {
          "inputs": "2e1b73fac4826d0a",
          "steps": [
            {
              "operation": "normalise",
              "params": [
                {
                  "name": "of",
                  "kind": "text",
                  "value": "surface"
                }
              ]
            }
          ]
        }
      },
      {
        "key": {
          "schema": {
            "name": "example.repetition-key",
            "version": 1
          },
          "value": {
            "person": 1,
            "stimulus": "b",
            "phase": "repeat",
            "repeat": 2
          }
        },
        "values": [
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.125,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625
        ],
        "provenance": {
          "inputs": "7b00cbca4ae8762a",
          "steps": [
            {
              "operation": "normalise",
              "params": [
                {
                  "name": "of",
                  "kind": "text",
                  "value": "surface"
                }
              ]
            }
          ]
        }
      },
      {
        "key": {
          "schema": {
            "name": "example.repetition-key",
            "version": 1
          },
          "value": {
            "person": 2,
            "stimulus": "a",
            "phase": "repeat",
            "repeat": 0
          }
        },
        "values": [
          0.0625,
          0.0625,
          0.125,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625
        ],
        "provenance": {
          "inputs": "6c2d03920877ae2a",
          "steps": [
            {
              "operation": "normalise",
              "params": [
                {
                  "name": "of",
                  "kind": "text",
                  "value": "surface"
                }
              ]
            }
          ]
        }
      },
      {
        "key": {
          "schema": {
            "name": "example.repetition-key",
            "version": 1
          },
          "value": {
            "person": 2,
            "stimulus": "a",
            "phase": "repeat",
            "repeat": 1
          }
        },
        "values": [
          0.0625,
          0.0625,
          0.0625,
          0.125,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625
        ],
        "provenance": {
          "inputs": "c838e1141812528a",
          "steps": [
            {
              "operation": "normalise",
              "params": [
                {
                  "name": "of",
                  "kind": "text",
                  "value": "surface"
                }
              ]
            }
          ]
        }
      },
      {
        "key": {
          "schema": {
            "name": "example.repetition-key",
            "version": 1
          },
          "value": {
            "person": 2,
            "stimulus": "a",
            "phase": "repeat",
            "repeat": 2
          }
        },
        "values": [
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.125,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625
        ],
        "provenance": {
          "inputs": "b8408046476220aa",
          "steps": [
            {
              "operation": "normalise",
              "params": [
                {
                  "name": "of",
                  "kind": "text",
                  "value": "surface"
                }
              ]
            }
          ]
        }
      },
      {
        "key": {
          "schema": {
            "name": "example.repetition-key",
            "version": 1
          },
          "value": {
            "person": 2,
            "stimulus": "b",
            "phase": "repeat",
            "repeat": 0
          }
        },
        "values": [
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.125,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625
        ],
        "provenance": {
          "inputs": "2e1b73fac4826d0a",
          "steps": [
            {
              "operation": "normalise",
              "params": [
                {
                  "name": "of",
                  "kind": "text",
                  "value": "surface"
                }
              ]
            }
          ]
        }
      },
      {
        "key": {
          "schema": {
            "name": "example.repetition-key",
            "version": 1
          },
          "value": {
            "person": 2,
            "stimulus": "b",
            "phase": "repeat",
            "repeat": 1
          }
        },
        "values": [
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.125,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625
        ],
        "provenance": {
          "inputs": "7b00cbca4ae8762a",
          "steps": [
            {
              "operation": "normalise",
              "params": [
                {
                  "name": "of",
                  "kind": "text",
                  "value": "surface"
                }
              ]
            }
          ]
        }
      },
      {
        "key": {
          "schema": {
            "name": "example.repetition-key",
            "version": 1
          },
          "value": {
            "person": 2,
            "stimulus": "b",
            "phase": "repeat",
            "repeat": 2
          }
        },
        "values": [
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.125,
          0.0625,
          0.0625,
          0.0625,
          0.0625,
          0.0625
        ],
        "provenance": {
          "inputs": "f3819cf16cbf408a",
          "steps": [
            {
              "operation": "normalise",
              "params": [
                {
                  "name": "of",
                  "kind": "text",
                  "value": "surface"
                }
              ]
            }
          ]
        }
      }
    ],
    "inputHash": "26dd8b9539bed70d",
    "planHash": "18c9fcde538303e3"
  }
}
"""
  // format: on
