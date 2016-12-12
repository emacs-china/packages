include Nanoc::Helpers::HTMLEscape

def count_json file
  JSON.parse(open(file).read).length
end
