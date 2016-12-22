include Nanoc::Helpers::HTMLEscape

require 'json'

def count_json file
  JSON.parse(open(file).read).length
end

def pkg_name
  File.basename item.identifier.without_ext
end

def pkg_homepage json_str
  json = JSON.parse(json_str)
  if json["props"] && json["props"]["url"]
    # TODO Clickable
    json["props"]["url"]
  else
    '(none)'
  end
end

def pkg_deps json_str
  json = JSON.parse(json_str)
  if json["deps"]
    json["deps"].keys.join(', ')
  else
    '(none)'
  end
end

def pkg_vers json_str
  json = JSON.parse(json_str)
  if json["vers"]
    # TODO Show version number as well
    json["vers"].keys.join(', ')
  else
    '(none)'
  end
end
