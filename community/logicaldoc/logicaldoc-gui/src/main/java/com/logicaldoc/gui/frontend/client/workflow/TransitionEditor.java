package com.logicaldoc.gui.frontend.client.workflow;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.HeaderControl;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is the form used for the workflow transition settings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class TransitionEditor extends Window {

	private StateWidget widget;

	public TransitionEditor(StateWidget widget) {
		this.widget = widget;

		HeaderControl closeIcon = new HeaderControl(HeaderControl.CLOSE, new ClickHandler() {
			public void onClick(ClickEvent event) {
				destroy();
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, closeIcon);
		setTitle(I18N.message("editworkflowstate", I18N.message("transition")));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setMargin(3);
		setWidth(600);
		setHeight(400);
		centerInPage();

		TextAreaItem automation = ItemFactory.newTextAreaItemForAutomation("automation", "execscriptontranschosen",
				widget.getTransition().getOnChosen(), null, false);
		automation.setWidth("*");
		automation.setHeight("*");
		automation.setWrapTitle(false);

		final DynamicForm automationForm = new DynamicForm();
		automationForm.setTitleOrientation(TitleOrientation.TOP);
		automationForm.setNumCols(1);
		automationForm.setWidth100();
		automationForm.setHeight100();
		automationForm.setItems(automation);

		ToolStrip toolStrip = new ToolStrip();
		toolStrip.setHeight(20);
		toolStrip.setWidth100();
		toolStrip.addSpacer(2);

		final TextItem name = ItemFactory.newTextItem("name", "name", widget.getTransition().getText());
		name.setRequired(true);

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {
			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				if (name.validate()) {
					TransitionEditor.this.widget.getTransition().setText(name.getValue().toString().trim());
					TransitionEditor.this.widget.setContents(name.getValue().toString().trim());
					TransitionEditor.this.widget.getTransition()
							.setOnChosen(automationForm.getValueAsString("automation"));
					destroy();
				}
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

		toolStrip.addFormItem(name);
		toolStrip.addButton(save);
		toolStrip.addSeparator();
		toolStrip.addButton(close);
		addItem(toolStrip);
		addItem(automationForm);
	}
}