package com.logicaldoc.gui.frontend.client.metadata.stamp;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIStamp;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.frontend.client.services.StampService;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects details about a stamp
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class StampDetailsPanel extends VLayout {
	private GUIStamp stamp;

	private EditingTabSet tabSet;

	private StampsPanel stampsPanel;

	private Layout propertiesTabPanel;

	private StampProperties propertiesPanel;

	private Layout parametersTabPanel;

	private StampParameters parametersPanel;

	private Layout usersTabPanel;

	private StampUsersPanel usersPanel;

	public StampDetailsPanel(StampsPanel stampsPanel) {
		super();

		this.stampsPanel = stampsPanel;
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
				if (stamp.getId() != 0) {
					StampService.Instance.get().getStamp(stamp.getId(), new AsyncCallback<GUIStamp>() {
						@Override
						public void onFailure(Throwable caught) {
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(GUIStamp stamp) {
							setStamp(stamp);
						}
					});
				} else {
					GUIStamp newStamp = new GUIStamp();
					setStamp(newStamp);
				}
				tabSet.hideSave();
			}
		});

		Tab propertiesTab = new Tab(I18N.message("properties"));
		propertiesTabPanel = new HLayout();
		propertiesTabPanel.setWidth100();
		propertiesTabPanel.setHeight100();
		propertiesTab.setPane(propertiesTabPanel);
		tabSet.addTab(propertiesTab);

		Tab parametersTab = new Tab(I18N.message("parameters"));
		parametersTabPanel = new HLayout();
		parametersTabPanel.setWidth100();
		parametersTabPanel.setHeight100();
		parametersTab.setPane(parametersTabPanel);
		tabSet.addTab(parametersTab);

		Tab usersTab = new Tab(I18N.message("users"));
		usersTabPanel = new HLayout();
		usersTabPanel.setWidth100();
		usersTabPanel.setHeight100();
		usersTab.setPane(usersTabPanel);
		tabSet.addTab(usersTab);

		addMember(tabSet);
	}

	private void refresh() {
		tabSet.hideSave();

		if (propertiesPanel != null) {
			propertiesPanel.destroy();
			if (propertiesTabPanel.contains(propertiesPanel))
				propertiesTabPanel.removeMember(propertiesPanel);
		}

		if (parametersPanel != null) {
			parametersPanel.destroy();
			if (parametersTabPanel.contains(parametersPanel))
				parametersTabPanel.removeMember(parametersPanel);
		}

		if (usersPanel != null) {
			usersPanel.destroy();
			if (usersTabPanel.contains(usersPanel))
				usersTabPanel.removeMember(usersPanel);
		}

		ChangedHandler changeHandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				onModified();
			}
		};

		propertiesPanel = new StampProperties(stamp, changeHandler);
		propertiesTabPanel.addMember(propertiesPanel);

		parametersPanel = new StampParameters(stamp, changeHandler);
		parametersTabPanel.addMember(parametersPanel);

		usersPanel = new StampUsersPanel(stamp.getId());
		usersTabPanel.addMember(usersPanel);
	}

	public GUIStamp getStamp() {
		return stamp;
	}

	public void setStamp(GUIStamp stamp) {
		this.stamp = stamp;
		refresh();
	}

	public void onModified() {
		tabSet.displaySave();
	}

	private boolean validate() {
		boolean propsValid = propertiesPanel.validate();
		if (!propsValid)
			tabSet.selectTab(0);
		boolean paramsValid = parametersPanel.validate();
		if (!propsValid)
			tabSet.selectTab(1);
		return propsValid && paramsValid;
	}

	public void onSave() {
		if (validate()) {
			StampService.Instance.get().save(stamp, new AsyncCallback<GUIStamp>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIStamp stamp) {
					tabSet.hideSave();
					StampDetailsPanel.this.setStamp(stamp);
					if (stamp != null)
						stampsPanel.updateRecord(stamp);
				}
			});
		}
	}
}