require 'rake'

currentDay = '08'

task :build do
  build currentDay
end

task :run1 => [:build] do
  runday currentDay, "data", 1
end

task :test1 => [:build] do
  runday currentDay, "test", 1
end

task :run2 => [:build] do
  runday currentDay, "data", 2
end

task :test2 => [:build] do
  runday currentDay, "test", 2
end

task :prun, [:day, :input, :stage] do |t, args|
  build args[:day]
  runday args[:day], args[:input], args[:stage]
end

def build(day)
  sh "cabal build advent-#{day}"
end

def runday(day, input, stage)
  sh "cabal exec advent-#{day} -- -i data/day#{day}/#{input}.txt -s #{stage}"
end

task :help => [:build] do
  sh "cabal exec advent-01 -- -h"
end