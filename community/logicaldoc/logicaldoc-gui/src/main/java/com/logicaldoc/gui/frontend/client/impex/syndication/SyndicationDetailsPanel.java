package com.logicaldoc.gui.frontend.client.impex.syndication;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUISyndication;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.frontend.client.services.SyndicationService;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
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

		tabSet = new EditingTabSet(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		}, new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (syndication.getId() != 0) {
					SyndicationService.Instance.get().getSyndication(syndication.getId(),
							new AsyncCallback<GUISyndication>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUISyndication share) {
									setSyndication(share);
								}
							});
				} else {
					GUISyndication newSyndication = new GUISyndication();
					setSyndication(newSyndication);
				}
				tabSet.hideSave();
			}
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
			if (standardTabPanel.contains(standardPanel))
				standardTabPanel.removeMember(standardPanel);
		}

		ChangedHandler changeHandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				onModified();
			}
		};
		standardPanel = new SyndicationStandardProperties(syndication, changeHandler);
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
			SyndicationService.Instance.get().save(syndication, new AsyncCallback<GUISyndication>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUISyndication syndication) {
					tabSet.hideSave();
					if (syndication != null) {
						syndicationsPanel.updateRecord(syndication);
						syndicationsPanel.showSyndicationDetails(syndication);
					}
				}
			});
		}
	}
}