require 'rake'

currentDay = '05'

task :build do
  sh "cabal build advent-#{currentDay}"
end

task :run1 => [:build] do
  sh "cabal exec advent-#{currentDay} -- data/day#{currentDay}/data.txt 1"
end

task :test1 => [:build] do
  sh "cabal exec advent-#{currentDay} -- data/day#{currentDay}/test.txt 1"
end

task :run2 => [:build] do
  sh "cabal exec advent-#{currentDay} -- data/day#{currentDay}/data.txt 2"
end

task :test2 => [:build] do
  sh "cabal exec advent-#{currentDay} -- data/day#{currentDay}/test.txt 2"
end