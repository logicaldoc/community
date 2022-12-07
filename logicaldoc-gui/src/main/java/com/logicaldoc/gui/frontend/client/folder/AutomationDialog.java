package com.logicaldoc.gui.frontend.client.folder;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAutomationRoutine;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.ExtendedPropertiesPanel;
import com.logicaldoc.gui.frontend.client.services.AutomationService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is the form used to execute an automation routine. You can write an
 * in-line script or call an existing routine. In this second case a form with
 * the input parameters is shown.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class AutomationDialog extends Window {

	private DynamicForm scriptForm = null;

	private SelectItem routineSelector;

	private GUIAutomationRoutine routine = new GUIAutomationRoutine();

	private ExtendedPropertiesPanel propertiesPanel;

	private TabSet tabSet = new TabSet();

	public AutomationDialog(final Long folderId, final long[] docIds) {
		HeaderControl closeIcon = new HeaderControl(HeaderControl.CLOSE, new ClickHandler() {
			public void onClick(ClickEvent event) {
				destroy();
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, closeIcon);
		setTitle(I18N.message("executeautomation"));

		setWidth("60%");
		setHeight(460);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		ToolStripButton execute = new ToolStripButton();
		execute.setTitle(I18N.message("execute"));
		execute.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onExecute(folderId, docIds);
			}
		});

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		close.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				destroy();
			}
		});

		tabSet.addTab(prepareScriptTab());
		tabSet.addTab(prepareParametersTab());
		tabSet.disableTab(1);

		ChangedHandler changeHandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				if (event == null
						|| (event != null && (event.getValue() == null || event.getValue().toString().isEmpty()))) {
					tabSet.enableTab(0);
					tabSet.selectTab(0);
					tabSet.disableTab(1);
					routine = new GUIAutomationRoutine();
				} else {
					AutomationService.Instance.get().getRoutine(Long.parseLong(event.getValue().toString()),
							new AsyncCallback<GUIAutomationRoutine>() {

								@Override
								public void onFailure(Throwable caught) {
									GuiLog.serverError(caught);
								}

								@Override
								public void onSuccess(GUIAutomationRoutine rt) {
									routine = rt;
									tabSet.enableTab(1);
									tabSet.selectTab(1);
									tabSet.disableTab(0);
									propertiesPanel = new ExtendedPropertiesPanel(routine, null, true, true, false);
									tabSet.getTab(1).setPane(propertiesPanel);
								}
							});
				}
			}
		};
		routineSelector = ItemFactory.newAutomationRoutineSelector("routine", null, changeHandler, true);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);
		toolStrip.addFormItem(routineSelector);
		toolStrip.addButton(execute);
		toolStrip.addSeparator();
		toolStrip.addButton(close);

		addItem(toolStrip);
		addItem(tabSet);
	}

	private Tab prepareScriptTab() {
		scriptForm = new DynamicForm();
		scriptForm.setWidth100();
		scriptForm.setHeight100();
		scriptForm.setTitleOrientation(TitleOrientation.TOP);
		scriptForm.setNumCols(1);

		final TextAreaItem automation = ItemFactory.newTextAreaItemForAutomation("automation", "automation", null, null,
				false);
		automation.setShowTitle(false);
		automation.setStartRow(false);
		automation.setRequired(true);
		automation.setWidth("*");
		automation.setHeight("*");
		scriptForm.setItems(automation);

		Tab tab = new Tab(I18N.message("script"));
		tab.setPane(scriptForm);
		return tab;
	}

	private Tab prepareParametersTab() {
		Tab tab = new Tab(I18N.message("parameters"));
		return tab;
	}

	private void onExecute(Long folderId, long[] docIds) {
		if (routine.getId() == 0L && !scriptForm.validate())
			return;

		if (routine.getId() != 0L && propertiesPanel != null && !propertiesPanel.validate())
			return;

		if (routine.getId() == 0L)
			routine.setAutomation(scriptForm.getValueAsString("automation"));

		AutomationService.Instance.get().execute(routine, docIds, folderId, new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(Void arg0) {
				AutomationDialog.this.destroy();
				GuiLog.info(I18N.message("automationlaunched"));
			}
		});
	}
}