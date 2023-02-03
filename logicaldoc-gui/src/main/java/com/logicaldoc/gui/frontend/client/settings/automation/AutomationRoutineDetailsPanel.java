package com.logicaldoc.gui.frontend.client.settings.automation;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
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
 * This panel collects details about an automation routine
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class AutomationRoutineDetailsPanel extends VLayout {
	private GUIAutomationRoutine routine;

	private Layout standardTabPanel;

	private AutomationRoutineProperties standardPanel;

	private Layout parametersTabPanel;

	private AutomationRoutineParametersPanel parametersPanel;

	private EditingTabSet tabSet;

	private AutomationRoutinesPanel routinesPanel;

	public AutomationRoutineDetailsPanel(AutomationRoutinesPanel foldersPanel) {
		super();

		this.routinesPanel = foldersPanel;
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
				if (routine.getId() != 0) {
					AutomationService.Instance.get().getRoutine(routine.getId(),
							new AsyncCallback<GUIAutomationRoutine>() {
								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUIAutomationRoutine routine) {
									setRoutine(routine);
								}
							});
				} else {
					GUIAutomationRoutine newRoutine = new GUIAutomationRoutine();
					setRoutine(newRoutine);
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

		Tab parametersTab = new Tab(I18N.message("parameters"));
		parametersTabPanel = new HLayout();
		parametersTabPanel.setWidth100();
		parametersTabPanel.setHeight100();
		parametersTab.setPane(parametersTabPanel);
		tabSet.addTab(parametersTab);

		addMember(tabSet);
	}

	private void refresh() {
		tabSet.hideSave();

		if (standardPanel != null) {
			standardPanel.destroy();
			if (Boolean.TRUE.equals(standardTabPanel.contains(standardPanel)))
				standardTabPanel.removeMember(standardPanel);
		}

		if (parametersPanel != null) {
			parametersPanel.destroy();
			if (Boolean.TRUE.equals(parametersTabPanel.contains(parametersPanel)))
				parametersTabPanel.removeMember(parametersPanel);
		}

		ChangedHandler changeHandler = (ChangedEvent event) -> onModified();

		standardPanel = new AutomationRoutineProperties(routine, changeHandler);
		standardTabPanel.addMember(standardPanel);

		parametersPanel = new AutomationRoutineParametersPanel(routine, changeHandler);
		parametersTabPanel.addMember(parametersPanel);
	}

	public GUIAutomationRoutine getTrigger() {
		return routine;
	}

	public void setRoutine(GUIAutomationRoutine trigger) {
		this.routine = trigger;
		refresh();
	}

	public void onModified() {
		tabSet.displaySave();
	}

	private boolean validate() {
		boolean stdValid = standardPanel.validate();
		if (!stdValid)
			tabSet.selectTab(0);
		boolean inputValid = parametersPanel.validate();
		if (!inputValid)
			tabSet.selectTab(1);
		return stdValid && inputValid;
	}

	public void onSave() {
		if (validate()) {
			AutomationService.Instance.get().saveRoutine(routine, new AsyncCallback<GUIAutomationRoutine>() {
				@Override
				public void onFailure(Throwable caught) {
					GuiLog.serverError(caught);
				}

				@Override
				public void onSuccess(GUIAutomationRoutine routine) {
					tabSet.hideSave();
					AutomationRoutineDetailsPanel.this.setRoutine(routine);
					if (routine != null)
						routinesPanel.updateRecord(routine);
				}
			});
		}
	}
}