package com.logicaldoc.gui.frontend.client.ai.filler;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects details about a filler
 * 
 * @author Giuseppe Desiato - LogicalDOC
 * @since 9.2.3
 */
public class FillerDetailsPanel extends VLayout {

	private GUIFiller filler;

	private Layout standardTabPanel;

	private FillerProperties standardPanel;

	private EditingTabSet tabSet;

	private FillersPanel fillersPanel;

	public FillerDetailsPanel(FillersPanel fillersPanel) {
		super();

		this.fillersPanel = fillersPanel;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new EditingTabSet(saveEvent -> onSave(), cancelEvent -> {
			if (filler.getId() != 0) {
				FillerService.Instance.get().get(filler.getId(), new DefaultAsyncCallback<>() {

					@Override
					public void handleSuccess(GUIFiller filler) {
						setFiller(filler);
					}

				});
			} else {
				setFiller(new GUIFiller());
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

		standardPanel = new FillerProperties(filler, event -> onModified());
		standardTabPanel.addMember(standardPanel);

	}

	public GUIFiller getFiller() {
		return filler;
	}

	public void setFiller(GUIFiller filler) {
		this.filler = filler;
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
			FillerService.Instance.get().save(filler, new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(GUIFiller filler) {
					tabSet.hideSave();
					if (filler != null) {
						fillersPanel.updateRecord(filler);
						fillersPanel.showFillerDetails(filler);
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
