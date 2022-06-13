:- module(detectors, [detector/2, can_sense/3]).

%% An agent's detectors

detector(detector_color).
detector(detector_infrared_distance).
detector(detector_infrared_orientation).
detector(detector_ultrasound_distance).
detector(detector_luminance).
detector(detector_touch).

% A detector has no umwelt. It is strictly a sensor. it does not abduce objects.
% It senses properties of types of objects.

can_sense(detector_color, color_of, floor).
can_sense(detector_infrared_distance, distance_to, food).
can_sense(detector_infrared_orientation, direction_of, food).
can_sense(detector_ultrasound_distance, distance_to, obstacle).
can_sense(detector_luminance, brightness, floor).
can_sense(detector_touch, touching, obstacle).
