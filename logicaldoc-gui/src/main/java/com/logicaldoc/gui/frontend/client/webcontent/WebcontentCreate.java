package com.logicaldoc.gui.frontend.client.webcontent;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.document.DocumentsPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SubmitItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This popup window is used to create a new web content.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.7
 */
public class WebcontentCreate extends Window {
	private static final String TEMPLATE = "template";

	DynamicForm form = new DynamicForm();

	public WebcontentCreate() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("createwebcontent"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setAutoSize(true);
		centerInPage();

		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);

		TextItem title = ItemFactory.newTextItem("title", null);
		title.setRequired(true);
		title.setWidth(200);

		SelectItem template = ItemFactory.newTemplateSelector(true, null);

		SubmitItem create = new SubmitItem();
		create.setTitle(I18N.message("create"));
		create.addClickHandler(event -> onCreate());

		form.setItems(title, template, create);

		addItem(form);
	}

	public void onCreate() {
		if (!form.validate())
			return;
		GUIDocument vo = new GUIDocument();
		String title = form.getValueAsString("title").trim();
		if (title.lastIndexOf('.') != -1)
			title = title.substring(0, title.lastIndexOf('.'));

		if (form.getValueAsString(TEMPLATE) == null || "".equals(form.getValueAsString(TEMPLATE)))
			vo.setTemplateId(null);
		else {
			vo.setTemplateId(Long.parseLong(form.getValueAsString(TEMPLATE)));
		}

		vo.setType("html");
		vo.setFileName(title + ".html");
		vo.setStatus(1);
		vo.setLanguage(I18N.getDefaultLocaleForDoc());
		vo.setFolder(FolderController.get().getCurrentFolder());

		LD.contactingServer();
		DocumentService.Instance.get().createWithContent(vo, "<html><body></body></html>", true,
				new DefaultAsyncCallback<>() {
					@Override
					public void onFailure(Throwable caught) {
						super.onFailure(caught);
						destroy();
					}

					@Override
					public void onSuccess(GUIDocument document) {
						LD.clearPrompt();

						DocumentsPanel.get().refresh();
						DocumentsPanel.get().selectDocument(document.getId(), true);

						Session.get().getUser().setCheckedOutDocs(Session.get().getUser().getCheckedOutDocs() + 1);
						GuiLog.info(I18N.message("documentcheckedout"), null);

						destroy();

						new WebcontentEditor(document).show();
					}
				});

		destroy();
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