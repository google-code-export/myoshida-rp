
ActionController::Routing::Routes.draw do |map|
  map.with_options :controller => 'foos' do |foos|
    foos.connect 'projects/:project_id/foos', :action => 'index'
    foos.connect 'projects/:project_id/foos/:id', :action => 'show', :id => /\d+/
    foos.connect 'projects/:project_id/foos/:action/:id'
  end
end
