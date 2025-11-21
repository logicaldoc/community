package com.logicaldoc.gui.frontend.client.ai.embedding;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.frontend.client.ai.AIService;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects details about an embedding scheme
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.2
 */
public class EmbeddingSchemeDetailsPanel extends VLayout {

	private GUIEmbeddingScheme embeddingScheme;

	private Layout standardTabPanel;

	private EmbeddingSchemeProperties standardPanel;

	private EditingTabSet tabSet;

	private EmbeddingSchemesPanel embeddingSchemesPanel;

	public EmbeddingSchemeDetailsPanel(EmbeddingSchemesPanel embeddingSchemesPanel) {
		super();
		this.embeddingSchemesPanel = embeddingSchemesPanel;

		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new EditingTabSet(saveEvent -> onSave(), cancelEvent -> {
			if (embeddingScheme.getId() != 0) {
				AIService.Instance.get().getEmbeddingScheme(embeddingScheme.getId(), new DefaultAsyncCallback<>() {

					@Override
					public void handleSuccess(GUIEmbeddingScheme scheme) {
						setEmbeddingScheme(scheme);
					}
				});
			} else {
				setEmbeddingScheme(new GUIEmbeddingScheme());
			}
			tabSet.hideSave();
		});

		Tab propertiesTab = new Tab(I18N.message("properties"));
		standardTabPanel = new HLayout();
		standardTabPanel.setWidth100();
		standardTabPanel.setHeight100();
		propertiesTab.setPane(standardTabPanel);
		tabSet.addTab(propertiesTab);

		addMember(tabSet);
	}

	public GUIEmbeddingScheme getEmbeddingScheme() {
		return embeddingScheme;
	}

	public void setEmbeddingScheme(GUIEmbeddingScheme embeddingScheme) {
		this.embeddingScheme = embeddingScheme;
		refresh();
	}

	private void refresh() {
		tabSet.hideSave();

		if (standardPanel != null) {
			standardPanel.destroy();
			if (Boolean.TRUE.equals(standardTabPanel.contains(standardPanel)))
				standardTabPanel.removeMember(standardPanel);
		}

		standardPanel = new EmbeddingSchemeProperties(embeddingScheme, event -> onModified());
		standardTabPanel.addMember(standardPanel);

	}

	public void onModified() {
		tabSet.displaySave();
	}

	private boolean validate() {
		boolean stdValid = standardPanel.validate();
		if (!stdValid)
			tabSet.selectTab(0);
		return stdValid;
	}

	public void onSave() {
		if (validate()) {
			AIService.Instance.get().saveEmbeddingScheme(embeddingScheme, new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(GUIEmbeddingScheme embeddingScheme) {
					tabSet.hideSave();
					if (embeddingScheme != null) {
						embeddingSchemesPanel.updateRecord(embeddingScheme);
						embeddingSchemesPanel.showEmbeddingSchemeDetails(embeddingScheme);
					}
				}
			});
		}
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}
