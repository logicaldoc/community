package com.logicaldoc.gui.frontend.client.ai;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.ai.model.ModelsPanel;
import com.logicaldoc.gui.frontend.client.ai.sampler.SamplersPanel;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * Panel showing the panels for handling AI models and samplers
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ModelsAndSamplersPanel extends AdminPanel {
	public ModelsAndSamplersPanel() {
		super("models");

		body.setMembers(new ModelsPanel());

		Tab attributesTab = new Tab(I18N.message("samplers"));
		attributesTab.setPane(new SamplersPanel());

		tabs.addTab(attributesTab);
	}
}