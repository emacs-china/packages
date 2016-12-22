include Nanoc::Helpers::HTMLEscape

require 'json'

def count_json file
  JSON.parse(open(file).read).length
end

def pkg_name item
  File.basename item.identifier.without_ext
end
