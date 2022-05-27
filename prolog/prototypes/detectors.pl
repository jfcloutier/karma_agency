:- module(detectors, [detector/2, sense/3]).

%% An agent's detectors

detector(detector_color).
detector(detector_infrared_distance).
detector(detector_infrared_orientation).
detector(detector_ultrasound_distance).
detector(detector_luminance).
detector(detector_touch).

% A detector has no umwelt. It is strictly a sensor. it does not abduce objects.
% It senses properties of types of objects.

sense(detector_color, color_of, floor).
sense(detector_infrared_distance, distance_to, food).
sense(detector_infrared_orientation, direction_of, food).
sense(detector_ultrasound_distance, distance_to, obstacle).
sense(detector_luminance, brightness, floor).
sense(detector_touch, touching, obstacle).
