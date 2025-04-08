package com.logicaldoc.gui.frontend.client.ai.robot;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RichTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridRecord;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This dialog is used to edit the message template
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.3
 */
public class AnswerEditor extends Window {

	private static final String CATEGORY = "category";

	private static final String ANSWER = "answer";

	private ListGrid grid;

	private ListGridRecord rec;

	private DynamicForm form = new DynamicForm();
	
	private ChangedHandler changedHandler;

	public AnswerEditor(ListGrid grid, ListGridRecord rec, ChangedHandler changedHandler) {
		this.rec = rec;
		this.grid = grid;
		this.changedHandler = changedHandler;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message(ANSWER));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setMargin(3);
		setWidth(670);
		setHeight(600);
		centerInPage();

		ToolStripButton save = new ToolStripButton();
		save.setTitle(I18N.message("save"));
		save.addClickHandler(event -> onSave());

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

		TextItem category = ItemFactory.newSimpleTextItem(CATEGORY, rec.getAttributeAsString(CATEGORY));
		category.setRequired(true);
		category.setWidth("*");
		category.setHeight(30);

		RichTextItem answer = ItemFactory.newRichTextItemForAutomation(ANSWER, ANSWER, rec.getAttributeAsString(ANSWER),
				null);
		answer.setRequired(true);
		answer.setWidth("*");
		answer.setHeight("*");

		form.setWidth100();
		form.setHeight100();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);
		form.setItems(category, answer);

		addItem(form);
	}

	private void onSave() {
		if (form.validate()) {
			rec.setAttribute(CATEGORY, form.getValueAsString(CATEGORY));
			rec.setAttribute(ANSWER, form.getValueAsString(ANSWER));
			grid.refreshRow(grid.getRowNum(rec));
			changedHandler.onChanged(null);
			destroy();
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