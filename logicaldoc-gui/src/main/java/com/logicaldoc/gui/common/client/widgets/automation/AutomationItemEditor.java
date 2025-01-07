package com.logicaldoc.gui.common.client.widgets.automation;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.VStack;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This dialog is used to edit a form item that contains an automation script
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.3
 */
public class AutomationItemEditor extends Window {

	private AutomationEditor editor;

	private FormItem item;

	private ChangedHandler handler;

	private VStack automationPanel = new VStack();

	public AutomationItemEditor(FormItem item, ChangedHandler handler) {
		this.item = item;
		this.handler = handler;

		HeaderControl maximize = new HeaderControl(HeaderControl.MAXIMIZE, event -> maximize());

		HeaderControl minimize = new HeaderControl(HeaderControl.MINIMIZE, event -> resetDimensions());

		HeaderControl closeIcon = new HeaderControl(HeaderControl.CLOSE, event -> destroy());

		setHeaderControls(HeaderControls.HEADER_LABEL, maximize, minimize, closeIcon);
		setTitle(item.getTitle());
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setMargin(3);
		resetDimensions();

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(event -> {
			onSave();
			destroy();
		});

		ToolStripButton close = new ToolStripButton();
		close.setTitle(I18N.message("close"));
		close.addClickHandler(event -> destroy());

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);
		toolStrip.addButton(save);
		toolStrip.addSeparator();
		toolStrip.addButton(close);

		addItem(toolStrip);

		addItem(automationPanel);

		addResizedHandler(event -> prepareAutomationPanel());

		prepareAutomationPanel();
	}

	private void resetDimensions() {
		setWidth(680);
		setHeight(460);
		centerInPage();
	}

	private void prepareAutomationPanel() {
		String text = item.getValue() != null ? item.getValue().toString() : "";
		if (editor != null) {
			text = editor.getText();
			editor.destroy();
			automationPanel.removeMember(editor);
		}

		automationPanel.setWidth100();
		automationPanel.setHeight100();

		editor = new AutomationEditor(text, null);
		editor.setHeight(getHeight() - 75);

		automationPanel.setMembers(editor);
	}

	private void onSave() {
		item.setValue(editor.getText());
		if (handler != null)
			handler.onChanged(null);
	}

	@Override
	public void destroy() {
		editor.destroy();
		removeItem(automationPanel);
		super.destroy();
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}
}