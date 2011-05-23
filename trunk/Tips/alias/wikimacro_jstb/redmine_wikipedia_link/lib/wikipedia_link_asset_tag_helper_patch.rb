module ActionView
  module Helpers
    module AssetTagHelper
      def javascript_include_tag_with_wikipedia_link(*sources)
        out = javascript_include_tag_without_wikipedia_link(*sources)
        if sources.is_a?(Array) and sources[0] == 'jstoolbar/textile'
          out += javascript_tag <<-javascript_tag
jsToolBar.prototype.elements.wikipedia_link = {
	type: 'button',
        title: 'Link to Wikipedia',
	fn: {
		wiki: function() { this.encloseSelection("{{wikipedia(", ")}}") }
	}
}
javascript_tag
          out += stylesheet_link_tag 'wikipedialink', :plugin => 'redmine_wikipedia_link'
        end
        out
      end
      alias_method_chain :javascript_include_tag, :wikipedia_link
    end
  end
end
