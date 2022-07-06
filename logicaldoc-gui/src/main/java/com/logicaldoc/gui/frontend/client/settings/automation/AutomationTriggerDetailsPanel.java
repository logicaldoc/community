package com.logicaldoc.gui.frontend.client.settings.automation;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAutomationTrigger;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.widgets.EditingTabSet;
import com.logicaldoc.gui.frontend.client.services.AutomationService;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
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

		tabSet = new EditingTabSet(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onSave();
			}
		}, new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (trigger.getId() != 0) {
					AutomationService.Instance.get().getTrigger(trigger.getId(),
							new AsyncCallback<GUIAutomationTrigger>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUIAutomationTrigger trigger) {
									setTrigger(trigger);
								}
							});
				} else {
					GUIAutomationTrigger newTrigger = new GUIAutomationTrigger();
					setTrigger(newTrigger);
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
		standardPanel = new AutomationTriggerProperties(trigger, changeHandler);
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
			AutomationService.Instance.get().saveTrigger(trigger, new AsyncCallback<GUIAutomationTrigger>() {
				@Override
				public void onFailure(Throwable caught) {
					if (caught.getMessage().toLowerCase().contains("cron")) {
						standardPanel.invalidCronExpression(caught.getMessage());
					} else
						GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIAutomationTrigger trigger) {
					tabSet.hideSave();
					AutomationTriggerDetailsPanel.this.setTrigger(trigger);
					if (trigger != null)
						triggersPanel.updateRecord(trigger);
				}
			});
		}
	}
}