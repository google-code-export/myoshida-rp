# 
# report_item.rb
# 

class ReportItem

  @@captions = {}
  
  def self.items
    [:permissions, :workflows, :settings, :plugins, :info]
  end

  def self.hide_map
    map = {}
    ReportItem.items.each {|el|
      map['hide_' + el.to_s] = false
    }
  end

  def self.label(item)
    item = :info	unless item
    I18n.t(@@captions[item.to_sym])
  end

  
  def self.push_menu(menu, item, caption = nil, opts = {})
    url = {:controller => 'admin_reports', :action => :info}
    copts = opts.clone

    if (item != :info)
      url[:id] = item
      copts[:if] = Proc.new { ReportItem::is_shown?(item) }
    end

    if (caption)
      copts[:caption] = caption
    else
      caption = ("label_" + item.to_s).to_sym
    end
    @@captions[item] = caption

    menu.push(item, url, copts)
  end
    
  
  def self.is_shown?(item)
    hidekey = 'hide_'
    hidekey += (item) ? item.to_s : "info"
    return !Setting.plugin_redmine_administration_reports[hidekey];
  end

  
end
