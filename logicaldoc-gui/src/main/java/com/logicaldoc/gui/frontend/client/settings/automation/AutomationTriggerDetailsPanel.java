package com.logicaldoc.gui.frontend.client.settings.automation;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAutomationTrigger;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.frontend.client.services.AutomationService;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.Layout;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tab.Tab;

/**
 * This panel collects details about an automation trigger
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class AutomationTriggerDetailsPanel extends VLayout {
	private GUIAutomationTrigger trigger;

	private Layout standardTabPanel;

	private AutomationTriggerProperties standardPanel;

	private EditingTabSet tabSet;

	private AutomationTriggersPanel triggersPanel;

	public AutomationTriggerDetailsPanel(AutomationTriggersPanel foldersPanel) {
		super();

		this.triggersPanel = foldersPanel;
		setHeight100();
		setWidth100();
		setMembersMargin(10);

		tabSet = new EditingTabSet(saveEvent -> onSave(), cancelEvent -> {
			if (trigger.getId() != 0) {
				AutomationService.Instance.get().getTrigger(trigger.getId(), new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUIAutomationTrigger trigger) {
						setTrigger(trigger);
					}
				});
			} else {
				GUIAutomationTrigger newTrigger = new GUIAutomationTrigger();
				setTrigger(newTrigger);
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

		standardPanel = new AutomationTriggerProperties(trigger, (ChangedEvent event) -> onModified());
		standardTabPanel.addMember(standardPanel);
	}

	public GUIAutomationTrigger getTrigger() {
		return trigger;
	}

	public void setTrigger(GUIAutomationTrigger trigger) {
		this.trigger = trigger;
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
			AutomationService.Instance.get().saveTrigger(trigger, new DefaultAsyncCallback<>() {
				@Override
				public void onFailure(Throwable caught) {
					if (caught.getMessage().toLowerCase().contains("cron")) {
						standardPanel.invalidCronExpression(caught.getMessage());
					} else
						super.onFailure(caught);
				}

				@Override
				public void handleSuccess(GUIAutomationTrigger trigger) {
					tabSet.hideSave();
					AutomationTriggerDetailsPanel.this.setTrigger(trigger);
					if (trigger != null)
						triggersPanel.updateRecord(trigger);
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