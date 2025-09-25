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
 * This is the form used to move a folder into another path
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.5
 */
public class MoveDialog extends Dialog {
	public MoveDialog() {
		super();
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("move"));
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

		ToolStripButton move = new ToolStripButton(I18N.message("move"));
		move.setAutoFit(true);
		move.setMargin(1);
		move.addClickHandler(event -> {
			String shownName = FolderNavigator.get().getSelectedRecord().getAttributeAsString("name");
			List<Long> selection = FolderNavigator.get().getSelectedIds();
			if (!selection.isEmpty())
				shownName = selection.size() + " " + I18N.message("folders").toLowerCase();

			LD.ask(I18N.message("move"),
					I18N.message("moveask", shownName, folders.getSelectedRecord().getAttributeAsString("name")),
					(Boolean value) -> {
						if (Boolean.TRUE.equals(value)) {
							FolderNavigator.get().moveTo(
									Long.parseLong(folders.getSelectedRecord().getAttributeAsString("folderId")));
							destroy();
						}
					});
		});
		ToolStrip buttons = new ToolStrip();
		buttons.setWidth100();
		buttons.addButton(move);

		addMember(folders);
		addMember(buttons);
	}
}