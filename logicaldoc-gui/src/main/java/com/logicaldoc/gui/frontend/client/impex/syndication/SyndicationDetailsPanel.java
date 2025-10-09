package com.logicaldoc.gui.frontend.client.impex.syndication;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUISyndication;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.frontend.client.services.SyndicationService;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects details about a syndication
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1.2
 */
public class SyndicationDetailsPanel extends VLayout {
	private GUISyndication syndication;

	private Layout standardTabPanel;

	private SyndicationStandardProperties standardPanel;

	private EditingTabSet tabSet;

	private SyndicationsPanel syndicationsPanel;

	public SyndicationDetailsPanel(SyndicationsPanel syndicationsPanel) {
		super();

		this.syndicationsPanel = syndicationsPanel;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new EditingTabSet(saveEvent -> onSave(), cancelEvent -> {
			if (syndication.getId() != 0) {
				SyndicationService.Instance.get().getSyndication(syndication.getId(), new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUISyndication share) {
						setSyndication(share);
					}
				});
			} else {
				GUISyndication newSyndication = new GUISyndication();
				setSyndication(newSyndication);
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

		standardPanel = new SyndicationStandardProperties(syndication, event -> onModified());
		standardTabPanel.addMember(standardPanel);

	}

	public GUISyndication getShare() {
		return syndication;
	}

	public void setSyndication(GUISyndication share) {
		this.syndication = share;
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
			SyndicationService.Instance.get().save(syndication, new DefaultAsyncCallback<>() {
				@Override
				public void handleSuccess(GUISyndication syndication) {
					tabSet.hideSave();
					if (syndication != null) {
						syndicationsPanel.updateRecord(syndication);
						syndicationsPanel.showSyndicationDetails(syndication);
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