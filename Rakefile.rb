require 'rake'

currentDay = '07'

task :build do
  sh "cabal build advent-#{currentDay}"
end

task :run1 => [:build] do
  sh "cabal exec advent-#{currentDay} -- -i data/day#{currentDay}/data.txt -s 1"
end

task :test1 => [:build] do
  sh "cabal exec advent-#{currentDay} -- -i data/day#{currentDay}/test.txt -s 1"
end

task :run2 => [:build] do
  sh "cabal exec advent-#{currentDay} -- -i data/day#{currentDay}/data.txt -s 2"
end

task :test2 => [:build] do
  sh "cabal exec advent-#{currentDay} -- -i data/day#{currentDay}/test.txt -s 2"
end

task :help => [:build] do
  sh "cabal exec advent-#{currentDay} -- -h"
end