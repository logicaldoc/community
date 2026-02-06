package com.logicaldoc.gui.frontend.client.document;

import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
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

/**
 * This popup window is used to convert a document in different formats
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class ConversionDialog extends Window {

	private static final String DOWNLOAD = "download";

	private static final String ACTION = "action";

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

		final RadioGroupItem action = ItemFactory.newRadioGroup(ACTION, I18N.message(ACTION));
		action.setRequired(true);
		action.setEndRow(true);

		final SelectItem format = ItemFactory.newConversionFormatItem(document.getFileName());
		format.setEndRow(true);
		format.setRequired(true);

		final ButtonItem convert = new ButtonItem();
		convert.setStartRow(false);
		convert.setTitle(I18N.message("convert"));
		convert.setAutoFit(true);
		convert.addClickHandler(event -> onConvert());

		FolderService.Instance.get().getFolder(document.getFolder().getId(), false, false, false,
				new DefaultAsyncCallback<>() {
					@Override
					public void handleSuccess(GUIFolder folder) {
						convert.setDisabled(!folder.isDownload() && !folder.isWrite());

						LinkedHashMap<String, String> map = new LinkedHashMap<>();
						if (folder.isDownload())
							map.put(DOWNLOAD, I18N.message(DOWNLOAD));
						if (folder.isWrite())
							map.put("save", I18N.message("save"));
						action.setValueMap(map);
						action.setValue(DOWNLOAD);

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
		if ("save".equals(form.getValueAsString(ACTION))) {
			DocumentService.Instance.get().convert(document.getId(), document.getFileVersion(), format,
					new DefaultAsyncCallback<>() {
						@Override
						public void handleSuccess(GUIDocument doc) {
							if (MainPanel.get().isOnDocumentsTab() && FolderController.get().getCurrentFolder() != null)
								FolderNavigator.get().selectFolder(FolderController.get().getCurrentFolder().getId());
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
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}
	
	@Override
	public int hashCode() {
		return super.hashCode();
	}
}