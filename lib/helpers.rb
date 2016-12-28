include Nanoc::Helpers::HTMLEscape
include Nanoc::Helpers::LinkTo

require 'json'

$all_packages_json = JSON.parse(open("content/all.json").read)

$gnu_packages_json          = JSON.parse(open("content/gnu.json").read)
$org_packages_json          = JSON.parse(open("content/org.json").read)
$melpa_packages_json        = JSON.parse(open("content/melpa.json").read)
$melpa_stable_packages_json = JSON.parse(open("content/melpa-stable.json").read)
$marmalade_packages_json    = JSON.parse(open("content/marmalade.json").read)
$user42_packages_json       = JSON.parse(open("content/user42.json").read)
$sunrise_commander_packages_json     = JSON.parse(open("content/sunrise-commander.json").read)

def pkg_json(pkg_name, elpa_name)
  json = case elpa_name
         when 'gnu'
           $gnu_packages_json
         when 'melpa'
           $melpa_packages_json
         when 'melpa-stable'
           $melpa_stable_packages_json
         when 'marmalade'
           $marmalade_packages_json
         when 'org'
           $org_packages_json
         when 'user42'
           $user42_packages_json
         when 'sunrise-commander'
           $sunrise_commander_packages_json
         end
  json[pkg_name]
end

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
    json["deps"].map{ |k,v| (kv = k + '-' + v.join('.'); $all_packages_json.has_key?(k) ? link_to(kv, "../#{k}/") : kv) }.join(", ")
  else
    '(none)'
  end
end

def pkg_vers json_str
  json = JSON.parse(json_str)
  if json["vers"]
    name = pkg_name
    json["vers"].map { |k,v|
      elpa = k
      type = pkg_json(name, k)['type'] == "single" ? "el" : "tar"
      version = v.join('.')
      link_to(version, "https://elpa.emacs-china.org/#{elpa}/#{name}-#{version}.#{type}") + " (#{elpa})" }.join(", ")
  else
    '(none)'
  end
end

def pkg_full_desc pkg_name
  file = "content/pkgs/#{pkg_name}-readme.txt"
  if File.exist?(file)
    html_escape(open(file).read)
  else
    '(none)'
  end
end
