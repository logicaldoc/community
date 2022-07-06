package com.logicaldoc.gui.frontend.client.metadata.template;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * Panel showing the panels for handling templates and attribute sets
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.5
 */
public class TemplatesAndAttributesPanel extends AdminPanel {
	public TemplatesAndAttributesPanel() {
		super("templates");

		body.setMembers(new TemplatesPanel());

		Tab attributesTab = new Tab(I18N.message("attributesets"));
		attributesTab.setPane(new AttributeSetsPanel());

		tabs.addTab(attributesTab);
	}
}