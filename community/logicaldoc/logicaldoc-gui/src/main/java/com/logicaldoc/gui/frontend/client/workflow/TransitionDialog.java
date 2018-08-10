package com.logicaldoc.gui.frontend.client.workflow;

import java.util.Map;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

/**
 * This is the form used for the workflow transition settings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.4
 */
public class TransitionDialog extends Window {

	private ValuesManager vm = new ValuesManager();

	private DynamicForm form;

	private StateWidget widget;

	public TransitionDialog(StateWidget widget) {
		this.widget = widget;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("editworkflowstate", I18N.message("transition")));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setMargin(3);
		setWidth(500);
		setHeight(400);

		centerInPage();

		form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);
		form.setValuesManager(vm);

		TextItem name = ItemFactory.newTextItem("name", "name", widget.getTransition().getText());
		name.setRequired(true);

		TextAreaItem onCreation = ItemFactory.newTextAreaItem("onCreation", "execscriptontranschosen", widget
				.getTransition().getOnChosen());
		onCreation.setWidth("*");
		onCreation.setHeight(270);
		onCreation.setWrapTitle(false);

		ButtonItem save = new ButtonItem("save", I18N.message("save"));
		save.setAutoFit(true);
		save.addClickHandler(new ClickHandler() {
			@SuppressWarnings("unchecked")
			public void onClick(ClickEvent event) {
				Map<String, Object> values = (Map<String, Object>) vm.getValues();

				if (vm.validate()) {
					TransitionDialog.this.widget.getTransition().setText((String) values.get("name"));
					TransitionDialog.this.widget.setContents((String) values.get("name"));
					TransitionDialog.this.widget.getTransition().setOnChosen((String) values.get("onCreation"));

					destroy();
				}
			}
		});

		form.setItems(name, onCreation, save);
		addItem(form);
	}
}