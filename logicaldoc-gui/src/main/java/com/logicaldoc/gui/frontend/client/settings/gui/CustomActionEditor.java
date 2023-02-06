package com.logicaldoc.gui.frontend.client.settings.gui;

import com.logicaldoc.gui.common.client.beans.GUIMenu;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.security.MenuRightsPanel;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.tab.Tab;
import com.smartgwt.client.widgets.tab.TabSet;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This dialog is used to edit a Custom Action
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.3
 */
public class CustomActionEditor extends Window {

	private DynamicForm form = new DynamicForm();

	private GUIMenu action;

	private CustomActionsPanel panel;

	private SelectItem routine;

	private TabSet tabs = new TabSet();

	private MenuRightsPanel rightsPanel = null;

	public CustomActionEditor(GUIMenu action, CustomActionsPanel panel) {
		this.action = action;
		this.panel = panel;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("customaction") + " - " + action.getName());
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setMargin(3);
		setWidth(500);
		setHeight(400);

		centerInPage();

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSave();
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

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);
		toolStrip.addButton(save);
		toolStrip.addSeparator();
		toolStrip.addButton(close);

		addItem(toolStrip);

		RadioGroupItem enabled = ItemFactory.newBooleanSelector("enabled", "enabled");
		enabled.setRequired(true);
		enabled.setValue(action.isEnabled() ? "yes" : "no");

		TextItem name = ItemFactory.newTextItem("name", action.getName());
		name.setRequired(true);

		TextItem description = ItemFactory.newTextItem("description", action.getDescription());
		description.setRequired(false);
		description.setWidth(300);

		final TextAreaItem automation = ItemFactory.newTextAreaItemForAutomation("automation", 
				action.getAutomation(), null, false);
		automation.setShowTitle(false);
		automation.setStartRow(false);
		automation.setWidth("*");
		automation.setHeight("*");
		automation.setDisabled(action.getRoutineId() != null);

		ChangedHandler changeHandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				if (event == null) {
					automation.setDisabled(false);
				} else {
					automation.setDisabled(event.getValue() != null && !event.getValue().toString().isEmpty());
				}
			}
		};
		routine = ItemFactory.newAutomationRoutineSelector("routine",
				action.getRoutineId() != null ? action.getRoutineId() : null, changeHandler, true);

		form.setWidth100();
		form.setHeight100();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);
		form.setItems(enabled, name, description, routine, automation);

		Tab details = new Tab(I18N.message("details"));
		details.setPane(form);

		Tab security = new Tab(I18N.message("security"));
		rightsPanel = new MenuRightsPanel(action, false);
		security.setPane(rightsPanel);

		tabs.setTabs(details, security);
		addItem(tabs);
	}

	private void onSave() {
		if (form.validate()) {
			action.setName(form.getValueAsString("name").replace("/", ""));
			action.setDescription(form.getValueAsString("description"));
			action.setAutomation(form.getValueAsString("automation"));
			action.setEnabled("yes".equals(form.getValueAsString("enabled")));
			action.setType(2);

			Long routineId = null;
			if (routine.getValue() != null && !routine.getValueAsString().isEmpty())
				routineId = Long.parseLong(routine.getValueAsString());
			action.setRoutineId(routineId);

			try {
				if (rightsPanel != null)
					action.setRights(rightsPanel.getRights());
			} catch (Throwable t) {
				// Nothing to do
			}

			panel.update(action);
			destroy();
		} else {
			tabs.selectTab(0);
		}
	}
}