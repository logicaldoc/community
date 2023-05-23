package com.logicaldoc.gui.frontend.client.security;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIGroup;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects all groups details
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GroupDetailsPanel extends VLayout {
	private GUIGroup group;

	private Layout propertiesTabPanel;

	private Layout usersTabPanel;

	private GroupPropertiesPanel propertiesPanel;

	private EditingTabSet tabSet;

	private GroupsPanel groupsPanel;

	private GroupUsersPanel usersPanel;

	public GroupDetailsPanel(GroupsPanel groupsPanel) {
		super();
		this.groupsPanel = groupsPanel;

		setHeight100();
		setWidth100();

		tabSet = new EditingTabSet(saveEvent -> onSave(), cancelEvent -> {
			if (group.getId() != 0) {
				SecurityService.Instance.get().getGroup(group.getId(), new AsyncCallback<GUIGroup>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIGroup group) {
						setGroup(group);
					}
				});
			} else {
				setGroup(new GUIGroup());
			}
			tabSet.hideSave();
		});

		Tab propertiesTab = new Tab(I18N.message("properties"));
		propertiesTabPanel = new HLayout();
		propertiesTabPanel.setHeight100();
		propertiesTab.setPane(propertiesTabPanel);
		tabSet.addTab(propertiesTab);

		Tab usersTab = new Tab(I18N.message("users"));
		usersTabPanel = new HLayout();
		usersTabPanel.setHeight100();
		usersTab.setPane(usersTabPanel);
		tabSet.addTab(usersTab);

		addMember(tabSet);
	}

	public GroupsPanel getGroupsPanel() {
		return groupsPanel;
	}

	private void refresh() {
		tabSet.hideSave();

		/*
		 * Prepare the standard properties tab
		 */
		if (propertiesPanel != null) {
			propertiesPanel.destroy();
			if (Boolean.TRUE.equals(propertiesTabPanel.contains(propertiesPanel)))
				propertiesTabPanel.removeMember(propertiesPanel);
		}

		propertiesPanel = new GroupPropertiesPanel(group, (ChangedEvent event) -> onModified());
		propertiesTabPanel.addMember(propertiesPanel);

		/*
		 * Prepare the history tab
		 */
		if (usersPanel != null) {
			usersPanel.destroy();
			if (Boolean.TRUE.equals(usersTabPanel.contains(usersPanel)))
				usersTabPanel.removeMember(usersPanel);
		}
		usersPanel = new GroupUsersPanel(group.getId());
		usersTabPanel.addMember(usersPanel);
	}

	public GUIGroup getGroup() {
		return group;
	}

	public void setGroup(GUIGroup group) {
		this.group = group;
		refresh();
	}

	public void onModified() {
		tabSet.displaySave();
	}

	private boolean validate() {
		boolean stdValid = propertiesPanel.validate();
		if (!stdValid)
			tabSet.selectTab(0);
		return stdValid;
	}

	public void onSave() {
		if (validate()) {
			SecurityService.Instance.get().saveGroup(group, new AsyncCallback<GUIGroup>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIGroup group) {
					tabSet.hideSave();
					if (group != null) {
						groupsPanel.updateRecord(group);
						groupsPanel.showGroupDetails(group);
					}
				}
			});
		}
	}
}