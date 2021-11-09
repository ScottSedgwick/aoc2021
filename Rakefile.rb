require 'rake'

task :run => [:r01]
task :test => [:t01]

task :r01 do
  run '01'
end

task :t01 do
  test '01'
end

def run(day)
  sh "cabal build advent-#{day};cabal exec advent-#{day}"
end

def test(day)
  sh "cabal test advent-test-#{day}"
end