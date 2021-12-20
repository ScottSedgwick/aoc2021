require 'rake'

currentDay = '20'

task :build do
  build currentDay
end

task :run1 do
  run currentDay, "data", 1
end

task :test1 do
  run currentDay, "test", 1
end

task :run2 do
  run currentDay, "data", 2
end

task :test2 do
  run currentDay, "test", 2
end

task :run, [:day, :input, :stage] do |t, args|
  run args[:day], args[:input], args[:stage]
end

def run(day, input, stage)
  build(day)
  runday(day, input, stage)
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