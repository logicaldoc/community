package com.logicaldoc.gui.frontend.client.ai.embedding;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * Panel showing the panels for handling AI embedding schemes and vector stores
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.2
 */
public class EmbeddingsAndVectorStorePanel extends AdminPanel {
	public EmbeddingsAndVectorStorePanel() {
		super("schemes");

		body.setMembers(new EmbeddingSchemesPanel());

		Tab storesTab = new Tab(I18N.message("vectorstores"));
		storesTab.setPane(new VectorStoresPanel());

		tabs.addTab(storesTab);
	}
}
