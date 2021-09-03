module Client.Util where
  
import Core.Data.Music (Note(..))


noteBackgroundClass :: Note -> String
noteBackgroundClass C = " bg-opacity-10 bg-red-600 "
noteBackgroundClass Db = " bg-opacity-10 bg-red-400 "
noteBackgroundClass D = " bg-opacity-10 bg-orange-500 "
noteBackgroundClass Eb = " bg-opacity-10 bg-amber-400 "
noteBackgroundClass E = " bg-opacity-10 bg-yellow-300 "
noteBackgroundClass F = " bg-opacity-10 bg-lime-500 "
noteBackgroundClass Gb = " bg-opacity-10 bg-green-500 "
noteBackgroundClass G = " bg-opacity-10 bg-teal-400 "
noteBackgroundClass Ab = " bg-opacity-10 bg-cyan-500 "
noteBackgroundClass A = " bg-opacity-10 bg-blue-400 "
noteBackgroundClass Bb = " bg-opacity-10 bg-violet-500 "
noteBackgroundClass B = " bg-opacity-10 bg-fuchsia-600 "

noteBorderClass :: Note -> String
noteBorderClass C = " border-red-600 "
noteBorderClass Db = " border-red-400 "
noteBorderClass D = " border-orange-500 "
noteBorderClass Eb = " border-amber-400 "
noteBorderClass E = " border-yellow-300 "
noteBorderClass F = " border-lime-500 "
noteBorderClass Gb = " border-green-500 "
noteBorderClass G = " border-teal-400 "
noteBorderClass Ab = " border-cyan-500 "
noteBorderClass A = " border-blue-400 "
noteBorderClass Bb = " border-violet-500 "
noteBorderClass B = " border-fuchsia-600 "

noteColorClass :: Note -> String
noteColorClass C = " text-red-600 "
noteColorClass Db = " text-red-400 "
noteColorClass D = " text-orange-500 "
noteColorClass Eb = " text-amber-400 "
noteColorClass E = " text-yellow-300 "
noteColorClass F = " text-lime-500 "
noteColorClass Gb = " text-green-500 "
noteColorClass G = " text-teal-400 "
noteColorClass Ab = " text-cyan-500 "
noteColorClass A = " text-blue-400 "
noteColorClass Bb = " text-violet-500 "
noteColorClass B = " text-fuchsia-600 "