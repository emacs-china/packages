include Nanoc::Helpers::HTMLEscape
include Nanoc::Helpers::LinkTo

require 'json'

$all_packages_json = JSON.parse(open("content/all.json").read)

def count_json file
  JSON.parse(open(file).read).length
end

def pkg_name
  File.basename item.identifier.without_ext
end

def pkg_homepage json_str
  json = JSON.parse(json_str)
  if json["props"] && json["props"]["url"]
    link_to(json["props"]["url"], json["props"]["url"])
  else
    '(none)'
  end
end

def pkg_deps json_str
  json = JSON.parse(json_str)
  if json["deps"]
    json["deps"].map{|k,v| (kv = k + '-' + v.join('.'); $all_packages_json.has_key?(k) ? link_to(kv, "../#{k}/") : kv)}.join(", ")
  else
    '(none)'
  end
end

def pkg_vers json_str
  json = JSON.parse(json_str)
  if json["vers"]
    name = pkg_name
    type = json["type"] == "single" ? "el" : "tar"
    json["vers"].map{|k,v| version = v.join('.'); link_to(version, "https://elpa.emacs-china.org/#{k}/#{name}-#{version}.#{type}") + " (#{k})"}.join(", ")
  else
    '(none)'
  end
end

def pkg_full_desc pkg_name
  file = "content/pkgs/" + pkg_name + "-readme.txt"
  if File.exist?(file)
    html_escape(open(file).read)
  else
    '(none)'
  end
end
