package com.logicaldoc.gui.frontend.client.document;

import java.util.LinkedHashMap;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.observer.FolderController;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.folder.FolderNavigator;
import com.logicaldoc.gui.frontend.client.panels.MainPanel;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.logicaldoc.gui.frontend.client.services.FolderService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.events.ClickEvent;
import com.smartgwt.client.widgets.form.fields.events.ClickHandler;

/**
 * This popup window is used to convert a document in different formats
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class ConversionDialog extends Window {

	private GUIDocument document;

	private DynamicForm form = new DynamicForm();

	public ConversionDialog(GUIDocument document) {
		this.document = document;
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("convert") + " - " + document.getFileName());
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		final RadioGroupItem action = ItemFactory.newRadioGroup("action", I18N.message("action"));
		action.setRequired(true);
		action.setEndRow(true);

		final SelectItem format = ItemFactory.newConversionFormatItem(document.getFileName());
		format.setEndRow(true);
		format.setRequired(true);

		final ButtonItem convert = new ButtonItem();
		convert.setStartRow(false);
		convert.setTitle(I18N.message("convert"));
		convert.setAutoFit(true);
		convert.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				onConvert();
			}
		});

		FolderService.Instance.get().getFolder(document.getFolder().getId(), false, false, false,
				new AsyncCallback<GUIFolder>() {

					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(GUIFolder folder) {
						convert.setDisabled(!folder.isDownload() && !folder.isWrite());

						LinkedHashMap<String, String> map = new LinkedHashMap<>();
						if (folder.isDownload())
							map.put("download", I18N.message("download"));
						if (folder.isWrite())
							map.put("save", I18N.message("save"));
						action.setValueMap(map);
						action.setValue("download");

						form.setFields(format, action, convert);
						addItem(form);
					}
				});
	}

	public void onConvert() {
		if (!form.validate())
			return;

		String format = form.getValueAsString("format");

		LD.contactingServer();
		if ("save".equals(form.getValueAsString("action"))) {
			DocumentService.Instance.get().convert(document.getId(), document.getFileVersion(), format,
					new AsyncCallback<GUIDocument>() {

						@Override
						public void onFailure(Throwable caught) {
							LD.clearPrompt();
							GuiLog.serverError(caught);
						}

						@Override
						public void onSuccess(GUIDocument doc) {
							LD.clearPrompt();
							if (MainPanel.get().isOnDocumentsTab())
								if (FolderController.get().getCurrentFolder() != null)
									FolderNavigator.get()
											.selectFolder(FolderController.get().getCurrentFolder().getId());
							destroy();
						}
					});
		} else {
			Util.download(Util.contextPath() + "convert?docId=" + document.getId() + "&fileVersion="
					+ document.getFileVersion() + "&format=" + format);
			LD.clearPrompt();
			destroy();
		}
	}
}