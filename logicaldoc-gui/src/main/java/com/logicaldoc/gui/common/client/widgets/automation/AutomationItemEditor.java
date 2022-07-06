package com.logicaldoc.gui.common.client.widgets.automation;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.events.ResizedEvent;
import com.smartgwt.client.widgets.events.ResizedHandler;
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

		HeaderControl maximize = new HeaderControl(HeaderControl.MAXIMIZE, new ClickHandler() {
			public void onClick(ClickEvent event) {
				maximize();
			}
		});

		HeaderControl minimize = new HeaderControl(HeaderControl.MINIMIZE, new ClickHandler() {
			public void onClick(ClickEvent event) {
				resetDimensions();
			}
		});

		HeaderControl closeIcon = new HeaderControl(HeaderControl.CLOSE, new ClickHandler() {
			public void onClick(ClickEvent event) {
				destroy();
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, maximize, minimize, closeIcon);
		setTitle(item.getTitle());
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setMargin(3);
		resetDimensions();

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSave();
				destroy();
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

		addItem(automationPanel);

		addResizedHandler(new ResizedHandler() {

			@Override
			public void onResized(ResizedEvent event) {
				prepareAutomationPanel();
			}
		});

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
}