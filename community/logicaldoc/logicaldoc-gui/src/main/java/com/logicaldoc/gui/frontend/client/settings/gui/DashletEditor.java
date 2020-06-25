package com.logicaldoc.gui.frontend.client.settings.gui;

import com.logicaldoc.gui.common.client.beans.GUIDashlet;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This dialog is used to edit the content of a dashlet
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.4
 */
public class DashletEditor extends Window {

	private DynamicForm form = new DynamicForm();

	private TextAreaItem content;

	private TextAreaItem query;

	private GUIDashlet dashlet;

	private DashletsPanel panel;

	public DashletEditor(GUIDashlet dashlet, DashletsPanel panel) {
		this.dashlet = dashlet;
		this.panel = panel;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("dashlet") + " - " + dashlet.getName());
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setMargin(3);
		setWidth(660);
		setHeight(600);

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

		TextItem title = ItemFactory.newTextItem("title", "title", dashlet.getTitle());
		title.setRequired(true);

		SpinnerItem max = ItemFactory.newSpinnerItem("max", "max", dashlet.getMax() != null ? dashlet.getMax() : 1);
		max.setMin(1);
		max.setRequired(true);

		SelectItem type = ItemFactory.newDashletTypeSelector(dashlet.getType());
		type.setRequired(true);
		type.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				onTypeChange(event.getValue().toString());
			}
		});

		content = ItemFactory.newTextAreaItemForAutomation("content", "content", dashlet.getContent(), null, true);
		content.setWidth("*");

		query = ItemFactory.newTextAreaItemForAutomation("query", "query", dashlet.getQuery(), null, false);
		query.setWidth("*");

		form.setWidth100();
		form.setHeight100();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);
		form.setItems(title, max, type, content, query);

		addItem(form);

		onTypeChange(dashlet.getType());
	}

	private void onTypeChange(String newValue) {
		if ("content".equals(newValue)) {
			content.show();
			query.hide();
		} else {
			content.hide();
			query.show();
		}
	}

	private void onSave() {
		if (form.validate()) {
			dashlet.setContent(form.getValueAsString("content"));
			dashlet.setTitle(form.getValueAsString("title"));
			dashlet.setQuery(form.getValueAsString("query"));
			dashlet.setType(form.getValueAsString("type"));
			dashlet.setMax(Integer.parseInt(form.getValueAsString("max")));
		}
		panel.refreshGrid();
		destroy();
	}
}