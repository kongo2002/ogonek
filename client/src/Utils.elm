-- Copyright 2018 Gregor Uhlenheuer
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Utils exposing (..)

import Time.DateTime


deltaToString : Time.DateTime.DateTimeDelta -> String
deltaToString delta =
  let fmt value unit = toString value ++ " " ++ unit
  in  if delta.days > 0 then
        fmt delta.days "days"
      else if delta.hours > 0 then
        fmt delta.hours "hours"
      else if delta.minutes > 0 then
        fmt delta.minutes "minutes"
      else
        fmt delta.seconds "seconds"


-- vim: et sw=2 sts=2
