package com.logicaldoc.gui.frontend.client.metadata.template;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.data.Record;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;
import com.smartgwt.client.widgets.grid.ListGrid;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * This popup window is used to define a new attribute's option.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.2
 */
public class AddAttributeOptionDialog extends Window {
	public AddAttributeOptionDialog(long setId, String attribute, ListGrid options) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("option"));
		setAutoSize(true);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		TextItem value = ItemFactory.newTextItem("value", "value", null);
		value.setWidth(200);
		value.setRequired(true);
		value.setWrapTitle(false);

		TextItem category = ItemFactory.newTextItem("category", "category", null);
		category.setWidth(200);
		category.setWrapTitle(false);

		ValuesManager vm = new ValuesManager();

		SubmitItem save = new SubmitItem();
		save.setTitle(I18N.message("save"));
		save.setAlign(Alignment.RIGHT);
		save.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (!vm.validate())
					return;

				Record rec = new ListGridRecord();
				rec.setAttribute("value", vm.getValueAsString("value").trim());
				if (vm.getValueAsString("category") != null)
					rec.setAttribute("category", vm.getValueAsString("category").trim());
				rec.setAttribute("setId", Long.toString(setId));
				rec.setAttribute("attribute", attribute);
				options.addData(rec);
				AddAttributeOptionDialog.this.destroy();
			}
		});

		DynamicForm form = new DynamicForm();
		form.setNumCols(1);
		form.setValuesManager(vm);
		form.setItems(value, category, save);

		addItem(form);
	}
}