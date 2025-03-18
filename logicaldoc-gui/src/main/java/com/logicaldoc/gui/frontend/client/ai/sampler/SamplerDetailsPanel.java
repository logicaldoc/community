package com.logicaldoc.gui.frontend.client.ai.sampler;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.frontend.client.ai.AIService;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects details about a sampler
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class SamplerDetailsPanel extends VLayout {
	private GUISampler sampler;

	private Layout standardTabPanel;

	private SamplerProperties standardPanel;

	private EditingTabSet tabSet;

	private SamplersPanel samplersPanel;

	public SamplerDetailsPanel(SamplersPanel samplersPanel) {
		super();

		this.samplersPanel = samplersPanel;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new EditingTabSet(saveEvent -> onSave(), cancelEvent -> {
			if (sampler.getId() != 0) {
				AIService.Instance.get().getSampler(sampler.getId(), new DefaultAsyncCallback<>() {

					@Override
					public void onSuccess(GUISampler sampler) {
						setSampler(sampler);
					}

				});
			} else {
				setSampler(new GUISampler());
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

		standardPanel = new SamplerProperties(sampler, event -> onModified());
		standardTabPanel.addMember(standardPanel);

	}

	public GUISampler getShare() {
		return sampler;
	}

	public void setSampler(GUISampler sampler) {
		this.sampler = sampler;
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
			AIService.Instance.get().saveSampler(sampler, new DefaultAsyncCallback<>() {
				@Override
				public void onSuccess(GUISampler sampler) {
					tabSet.hideSave();
					if (sampler != null) {
						samplersPanel.updateRecord(sampler);
						samplersPanel.showSamplerDetails(sampler);
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