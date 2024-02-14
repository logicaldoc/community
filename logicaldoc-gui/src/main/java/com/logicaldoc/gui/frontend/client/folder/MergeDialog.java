package com.logicaldoc.gui.frontend.client.folder;

import java.util.List;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.folder.browser.FolderBrowser;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Dialog;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is the form used to copy some folders into another target folder
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.1
 */
public class MergeDialog extends Dialog {
	private static final String MERGE = "merge";

	public MergeDialog() {
		super();
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message(MERGE));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setWidth(440);
		setHeight(500);

		final FolderBrowser folders = new FolderBrowser();
		folders.setWidth100();
		folders.setHeight100();
		folders.setMinHeight(300);

		ToolStripButton merge = new ToolStripButton(I18N.message(MERGE));
		merge.setAutoFit(true);
		merge.setMargin(1);
		merge.addClickHandler(event -> {
			List<Long> selectedIds = FolderNavigator.get().getSelectedIds();
			String label = FolderNavigator.get().getSelectedRecord().getAttributeAsString("name");
			if (!selectedIds.isEmpty())
				label = selectedIds.size() + " " + I18N.message("folders").toLowerCase();

			LD.ask(I18N.message(MERGE),
					I18N.message("mergeask", label, folders.getSelectedRecord().getAttributeAsString("name")),
					(Boolean value) -> {
						if (Boolean.TRUE.equals(value)) {
							FolderNavigator.get().mergeTo(
									Long.parseLong(folders.getSelectedRecord().getAttributeAsString("folderId")));
						}
						destroy();
					});
		});

		ToolStrip buttons = new ToolStrip();
		buttons.setWidth100();
		buttons.addButton(merge);

		addMember(folders);
		addMember(buttons);
	}
}