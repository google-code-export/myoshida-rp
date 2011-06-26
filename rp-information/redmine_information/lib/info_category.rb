# 
# info_category.rb
# 

class InfoCategory

  @@captions = {}
  
  def self.categories
    [:permissions, :workflows, :settings, :plugins, :version]
  end

  def self.hide_map
    map = {}
    InfoCategory.categories.each {|catsym|
      map['hide_' + catsym.to_s] = false
    }
    map
  end

  def self.label(catname)
    I18n.t(@@captions[catname.to_sym])
  end

  
  def self.push_menu(menu, catsym, caption = nil, opts = {})
    url = {:controller => :admin_reports, :action => :show}
    copts = opts.clone

    url[:id] = catsym
    copts[:if] = Proc.new { InfoCategory::is_shown?(catsym) }

    if (caption)
      copts[:caption] = caption
    else
      caption = ("label_" + catsym.to_s).to_sym
    end
    @@captions[catsym] = caption

    menu.push(catsym, url, copts)
  end
    
  
  def self.is_shown?(catsym)
    hidekey = 'hide_'
    hidekey += (catsym) ? catsym.to_s : "info"
    return !Setting.plugin_redmine_administration_reports[hidekey];
  end

  def self.do_nothing()
    "do_nothing"
  end
  
end
