module Lsl.EventSigs(lslEventDescriptors,simpleLslEventDescriptors, EventDelivery(..), EventAdditionalData(..)) where

import Lsl.Evaluation
import Lsl.Type

data EventDelivery = EventDeliveryScript | EventDeliveryPrim | EventDeliveryObject | EventDeliveryRoot
data EventAdditionalData = EventAdditionalKeys String String | EventAdditionalAvatarKeys String String | EventAdditionalVectors String String
                         | EventAdditionalInts String String

lslEventDescriptors = [ 
    ("at_rot_target", [(LLInteger,"tnum"),(LLRot,"targetRot"),(LLRot,"ourrot")], EventDeliveryScript, [],
     "raised when a scripted object comes within an angular distance of a target rotation"), 
    ("at_target", [(LLInteger,"tnum"),(LLVector,"targetPos"),(LLVector,"ourpos")], EventDeliveryScript, [],
     "raised when a scripted object comes within a distance of a target posisition"),
    ("attach",[(LLKey,"id")],  EventDeliveryRoot, [],
     "raised whenever the scripted object becomes attached or detached from an avatar"),
    ("changed", [(LLInteger,"change")], EventDeliveryObject, [],
     "raised whenever one of a variety of changes occur to a prim/object containing the script"),
    ("collision",[(LLInteger, "num_detected")], EventDeliveryPrim, [EventAdditionalKeys "Avatar/Object key" "Avatar/Object key for collision"],
     "raised when a scripted object collides with another object/avatar"),
    ("collision_end",[(LLInteger, "num_detected")],  EventDeliveryPrim, [EventAdditionalKeys "Avatar/Object key" "Avatar/Object key for collision"],
     "raised when a scripted object stops colliding with another object/avatar"),
    ("collision_start",[(LLInteger, "num_detected")], EventDeliveryPrim, [EventAdditionalKeys "Avatar/Object key" "Avatar/Object key for collision"],
     "raised when a scripted object starts colliding with another object/avatar"),
    ("control",[(LLKey,"id"),(LLInteger,"held"),(LLInteger,"change")], EventDeliveryScript, [],
     "raised when a the state of controls change, if the script has 'taken controls'"),
    ("dataserver",[(LLKey,"queryid"),(LLString,"data")], EventDeliveryScript, [],
     "raised when data is received from the dataserver (in response to one of a variety of ll-function calls)"),
    ("email",[(LLString,"time"),(LLString,"address"),(LLString,"subj"),(LLString,"message"),(LLInteger,"num_left")], EventDeliveryScript, [],
     "raised when the llGetNextEmail function call is answered"),
    ("http_response",[(LLKey,"request_id"),(LLInteger,"status"),(LLList,"metadata"),(LLString,"body")], EventDeliveryScript, [],
     "raised when an http response is received for an http request that is pending"),
    ("land_collision",[(LLVector,"pos")], EventDeliveryPrim, [],
     "raised when a scripted object is colliding with the ground"),
    ("land_collision_end",[(LLVector,"pos")], EventDeliveryPrim, [],
     "raised when a scripted object stops colliding with the ground"),
    ("land_collision_start",[(LLVector,"pos")], EventDeliveryPrim, [],
     "raised when a scripted object starts colliding with the ground"),
    ("link_message", [(LLInteger,"sender_num"), (LLInteger,"num"), (LLString,"str"), (LLKey,"id")], EventDeliveryPrim, [],
     "raised when a scripted prim receives a link message, sent via llMessageLinked"),
    ("listen", [(LLInteger, "channel"), (LLString,"name"), (LLKey,"id"), (LLString,"message")], EventDeliveryScript, [],
     "raised whenever 'chat' is detected that matches constraints specified via a llListen call"),
    ("money",[(LLKey,"id"),(LLInteger,"amount")], EventDeliveryPrim, [],
     "raised when an agent pays a Linden dollar amount to the object"),
    ("moving_end",[], EventDeliveryObject, [],
     "raised when a scripted object stops moving or has entered a different region"),
    ("moving_start",[], EventDeliveryObject, [],
     "raised when a scripted object starts moving or has entered a different region"),
    ("no_sensor",[], EventDeliveryScript, [],
     "raised when sensors are active, but are not sensing anything"),
    ("not_at_rot_target",[],  EventDeliveryScript, [],
     "raised when a scripted object is outside the specified angular distance of a target set by llRotTarget"),
    ("not_at_target",[], EventDeliveryScript, [],
     "raised when a scripted object is outside a specified range of a target set by llTarget"),
    ("object_rez",[(LLKey,"id")], EventDeliveryPrim, [],
     "raised when a script rezzes another object.  The id is the GUID of the object that it rezzed"),
    ("on_rez", [(LLInteger,"start_param")], EventDeliveryObject, [],
     "raised when an object rezzes (from an agents inventory, an objects inventory, or as an attachment when an user logs on)"),
    ("remote_data", [(LLInteger,"event_type"),(LLKey,"channel"),(LLKey,"message_id"),(LLString,"sender"),(LLInteger,"idata"),(LLString,"sdata")],
     EventDeliveryScript, [],
     "raised when something XML-RPC related happens"),
    ("run_time_permissions", [(LLInteger,"perm")], EventDeliveryScript, [],
     "raised when permissions granted to a script change, or a user responds to a permission request from the script"),
    ("sensor",[(LLInteger,"num_detected")], EventDeliveryScript, [EventAdditionalKeys "Avatar/Object key" "Sensed Avatar/Ojbect Key"],
     "raised when the sensor set up by llSensor/llSensorRepeat senses something"),
    ("state_entry", [], EventDeliveryScript, [],
     "raised when a state is entered"),
    ("state_exit", [], EventDeliveryScript, [],
     "raised when a state is exited"),
    ("timer", [], EventDeliveryScript, [],
     "raised when the timer set up by llSetTimerEvent triggers"),
    ("touch", [(LLInteger,"num_detected")], EventDeliveryPrim,
     [EventAdditionalAvatarKeys "Avatar key" "Key of agent touching prim",
      EventAdditionalVectors "Grab vector" "Force applied by agent grabbing prim",
      EventAdditionalInts "Link Number" "Link source of touch"],
     "raised when an agent touches a scripted object"),
    ("touch_start",[(LLInteger,"num_detected")], EventDeliveryPrim,
     [EventAdditionalAvatarKeys "Avatar key" "Key of agent touching prim",
      EventAdditionalInts "Link Number" "Link source of touch"],
     "raised when an agent starts touching a scripted object"),
    ("touch_end",[(LLInteger,"num_detected")], EventDeliveryPrim, 
     [EventAdditionalAvatarKeys "Avatar key" "Key of agent touching prim",
      EventAdditionalInts "Link Number" "Link source of touch"],
     "raised when an agent stops touching a scripted object")]

simpleLslEventDescriptors =
    map (\ (name,params,_,_,_) ->
            (name, map (\ (t,_) -> t) params)) lslEventDescriptors