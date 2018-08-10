package com.logicaldoc.gui.frontend.client.document;

import java.util.Map;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIBookmark;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.events.CloseClickEvent;
import com.smartgwt.client.widgets.events.CloseClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

public class BookmarkDialog extends Window {
	private ValuesManager vm = new ValuesManager();

	private GUIBookmark bookmark = null;

	public BookmarkDialog(GUIBookmark bookmk) {
		this.bookmark = bookmk;

		addCloseClickHandler(new CloseClickHandler() {
			@Override
			public void onCloseClick(CloseClickEvent event) {
				destroy();
			}
		});

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("bookmark"));
		setWidth(280);
		setHeight(250);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setPadding(5);
		setAutoSize(true);

		final DynamicForm form = new DynamicForm();
		form.setValuesManager(vm);
		form.setWidth(280);
		form.setMargin(5);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setNumCols(1);

		TextItem name = ItemFactory.newTextItem("name", "name", bookmark.getName());
		name.setRequired(true);

		TextItem description = ItemFactory.newTextItem("description", "description", bookmark.getDescription());

		ButtonItem save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.setAutoFit(true);

		save.addClickHandler(new ClickHandler() {
			@SuppressWarnings("unchecked")
			public void onClick(ClickEvent event) {
				Map<String, Object> values = (Map<String, Object>) vm.getValues();
				vm.validate();
				if (!vm.hasErrors()) {
					if ((String) values.get("name") != null && !((String) values.get("name")).trim().isEmpty()) {
						bookmark.setName((String) values.get("name"));
					}
					if ((String) values.get("description") != null) {
						bookmark.setDescription((String) values.get("description"));
					}

					DocumentService.Instance.get().updateBookmark(bookmark, new AsyncCallback<Void>() {
						@Override
						public void onFailure(Throwable caught) {
							Log.serverError(caught);
						}

						@Override
						public void onSuccess(Void ret) {
							destroy();
							BookmarksPanel.get().refresh();
						}
					});
				}
			}
		});

		form.setFields(name, description, save);
		addItem(form);
	}
}
