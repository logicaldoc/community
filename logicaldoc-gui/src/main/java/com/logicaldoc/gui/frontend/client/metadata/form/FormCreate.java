package com.logicaldoc.gui.frontend.client.metadata.form;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIForm;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.FormService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

/**
 * This popup window is used to create a new form.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class FormCreate extends Window {
	private SubmitItem create;

	private ValuesManager vm;

	private FormsPanel grid;

	public FormCreate(FormsPanel grid) {
		this.grid = grid;

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("createform"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		DynamicForm form = new DynamicForm();
		vm = new ValuesManager();
		form.setValuesManager(vm);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);

		TextItem name = ItemFactory.newSimpleTextItem("name", null);
		name.setRequired(true);
		name.setWidth(200);

		SelectItem template = ItemFactory.newTemplateSelector(true, null);

		create = new SubmitItem();
		create.setTitle(I18N.message("create"));
		create.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onCreate();
			}
		});

		form.setItems(name, template, create);

		addItem(form);
	}

	public void onCreate() {
		if (Boolean.FALSE.equals(vm.validate()))
			return;

		GUIForm vo = new GUIForm();
		vo.setName(vm.getValueAsString("name").trim());

		if (vm.getValueAsString("template") == null || "".equals(vm.getValueAsString("template").toString()))
			vo.setTemplateId(null);
		else
			vo.setTemplateId(Long.parseLong(vm.getValueAsString("template").toString()));

		FormService.Instance.get().save(vo, new AsyncCallback<GUIForm>() {
			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
				destroy();
			}

			@Override
			public void onSuccess(GUIForm form) {
				grid.refresh();
				destroy();
			}
		});

		destroy();
	}
}