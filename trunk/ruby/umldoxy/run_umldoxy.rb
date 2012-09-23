# 
# run_plantuml.rb
# 
# Author : Mitsuyoshi Yoshida
# This program is freely distributable under the terms of an MIT-style license.
# 

require 'tempfile'

DefaultImagePath = 'images'
DefaultAlias = [
  "ALIASES = startuml{1}=\"@image html \\1\\n@image rtf \\1\\n@image latex \\1\\n@if DontIgnorePlantUMLCode\"",
  "ALIASES += \"enduml=@endif\""
]
DefaultConfigFileName = "config.txt"
DefaultConfigFilePath = File.join(File.dirname(File.expand_path(__FILE__)), DefaultConfigFileName)

class DoxyfileParser
  
  NeedKeywords = ['INPUT', 'IMAGE_PATH', 'DOT_PATH', 'FILE_PATTERNS']
  KeywordPattern = /^(#{NeedKeywords.join('|')})\s*=\s*(.+)$/
  CommentPattern = /^\#/
  SuccPattern = /(.*)\\$/
  FilterPattern = /\*\.(\w+)/
  
  def initialize
    @prms = {}
  end

  def [](key)
    @prms[key]
  end
  
  def parse(file)
    curstr = nil
    File.open(file) {|fp|
      fp.each {|str|
        str.strip!
        next	if (str.empty? or str =~ CommentPattern)
        if (str =~ SuccPattern)
          if (curstr)
            curstr += $1
          else
            curstr = $1
          end
        else
          if (curstr)
            parse_line(curstr+str)
            curstr = nil
          else
            parse_line(str)
          end
        end
      }
    }
  end


  private
  
  def parse_line(str)
    return	unless str =~ KeywordPattern
    if ($1 == 'FILE_PATTERNS')
      ary = $2.scan(FilterPattern)
      if (ary and !ary.empty?)
        @prms['FILE_PATTERNS'] = "./**.(#{ary.join('|')})"
      end
    else
      @prms[$1] = File.expand_path($2.strip)
    end
  end

end


def run_command(prog, args)
  puts("  #{prog} " + args.join(' '))
  STDOUT.flush
  system(prog, *args)
  if ($?.to_i != 0)
    exit($?)
  end
end



def usage
  puts File.basename(__FILE__) + " PLANTUML_JAR_FILE [Doxyfile]"
end

def quote(str)
  '"' + str + '"'
end


##  Parse command argument

jarfile = ARGV.shift
unless jarfile
  usage
  exit(1)
end
jarfile = File.expand_path(jarfile)

doxyfile = ARGV.shift
doxyfile = 'Doxyfile'	unless doxyfile
unless (File.exist?(doxyfile))
  puts("No such a file. : " + quote(doxyfile))
  usage
  exit(1)
end
doxyfile = File.expand_path(doxyfile)
doxydir = File.dirname(doxyfile)

srcdir = nil
args = nil
parser = nil

# cd Doxyfile directory
Dir.chdir(doxydir) {

  ##  Parse Doxyfile
  puts "Parse Doxyfile : " + doxyfile

  parser = DoxyfileParser.new

  parser.parse(doxyfile)



  ## Execute PlantUML
  puts "Execute PlantUML : "

  args = ['-jar', jarfile]

  if (File.exist?(DefaultConfigFilePath))
    args += ['-config', DefaultConfigFilePath]
  end

  imgdir = parser['IMAGE_PATH']
  unless imgdir
    imgdir = File.expand_path(DefaultImagePath)
  end
  args += ['-o', imgdir]

  if (parser['DOT_PATH'])
    args += ['-graphvizdot', parser['DOT_PATH']]
  end

  args << parser['FILE_PATTERNS']

  srcdir = parser['INPUT']
  srcdir = File.expand_path('.')	unless srcdir

}

# cd src directory
Dir.chdir(srcdir) {
  run_command('java', args)
}



# cd Doxyfile directory
Dir.chdir(doxydir) {
  contents = File.read(doxyfile)
  tmpfp = Tempfile.open(File.basename(doxyfile))
  tmpfp.puts(contents)
  unless parser['IMAGE_PATH']
    tmpfp.puts('IMAGE_PATH = ' + DefaultImagePath)
  end
  unless parser['ALIASES']
    DefaultAlias.each {|str|
      tmpfp.puts(str)
    }
  end
  tmpfp.close
      
  ## Execute Doxygen
  puts "Execute Doxygen : "
  run_command('doxygen', [tmpfp.path])  
}

