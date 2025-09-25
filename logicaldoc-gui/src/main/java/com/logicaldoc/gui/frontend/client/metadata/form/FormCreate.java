package com.logicaldoc.gui.frontend.client.metadata.form;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIForm;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.menu.QuickSearchTray;
import com.logicaldoc.gui.frontend.client.services.FormService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This popup window is used to create a new form.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class FormCreate extends Window {

	private static final String TEMPLATE = "template";

	private DynamicForm form = new DynamicForm();

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

		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);

		TextItem name = ItemFactory.newSimpleTextItem("name", null);
		name.setRequired(true);
		name.setWidth(200);

		SelectItem template = ItemFactory.newTemplateSelector(true, null);

		SubmitItem create = new SubmitItem();
		create.setTitle(I18N.message("create"));
		create.addClickHandler(event -> onCreate());

		form.setItems(name, template, create);

		addItem(form);
	}

	public void onCreate() {
		if (!form.validate())
			return;

		GUIForm vo = new GUIForm();
		vo.setName(form.getValueAsString("name").trim());

		if (form.getValueAsString(TEMPLATE) == null || "".equals(form.getValueAsString(TEMPLATE)))
			vo.setTemplateId(null);
		else
			vo.setTemplateId(Long.parseLong(form.getValueAsString(TEMPLATE)));

		FormService.Instance.get().save(vo, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(GUIForm form) {
				grid.refresh();
				destroy();
			}
		});

		destroy();
	}

	@Override
	public boolean equals(Object obj) {
		if (obj instanceof QuickSearchTray)
			return super.equals(obj);
		else
			return false;
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}