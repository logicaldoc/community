package com.logicaldoc.gui.frontend.client.ai.model;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.frontend.client.ai.AIService;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects details about a model
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ModelDetailsPanel extends VLayout {
	private GUIModel model;

	private Layout standardTabPanel;

	private ModelProperties standardPanel;

	private EditingTabSet tabSet;

	private ModelsPanel modelsPanel;

	public ModelDetailsPanel(ModelsPanel samplersPanel) {
		super();

		this.modelsPanel = samplersPanel;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new EditingTabSet(saveEvent -> onSave(), cancelEvent -> {
			if (model.getId() != 0) {
				AIService.Instance.get().getModel(model.getId(), new DefaultAsyncCallback<>() {

					@Override
					public void onSuccess(GUIModel sampler) {
						setModel(model);
					}

				});
			} else {
				setModel(new GUIModel());
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

	private void refresh() {
		tabSet.hideSave();

		/*
		 * Prepare the standard properties tab
		 */
		if (standardPanel != null) {
			standardPanel.destroy();
			if (Boolean.TRUE.equals(standardTabPanel.contains(standardPanel)))
				standardTabPanel.removeMember(standardPanel);
		}

		standardPanel = new ModelProperties(model, event -> onModified());
		standardTabPanel.addMember(standardPanel);

	}

	public GUIModel getModel() {
		return model;
	}

	public void setModel(GUIModel model) {
		this.model = model;
		refresh();
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
			AIService.Instance.get().saveModel(model, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUIModel model) {
					tabSet.hideSave();
					if (model != null) {
						modelsPanel.updateRecord(model);
						modelsPanel.showModelDetails(model);
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