package com.logicaldoc.gui.frontend.client.ai.embedding;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.logicaldoc.gui.frontend.client.ai.sampler.SamplersPanel;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * Panel showing the panels for handling AI embedding schemes and vector stores
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.2
 */
public class EmbeddingsAndVectorsPanel extends AdminPanel {
	public EmbeddingsAndVectorsPanel() {
		super("embeddings");

		body.setMembers(new EmbeddingSchemesPanel());

		Tab samplersTab = new Tab(I18N.message("samplers"));
		samplersTab.setPane(new SamplersPanel());

		tabs.addTab(samplersTab);
	}
}
