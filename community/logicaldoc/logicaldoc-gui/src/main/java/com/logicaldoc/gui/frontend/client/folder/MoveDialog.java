package com.logicaldoc.gui.frontend.client.folder;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.common.client.widgets.FolderTree;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.util.BooleanCallback;
import com.smartgwt.client.widgets.Button;
import com.smartgwt.client.widgets.Dialog;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.layout.VLayout;
import com.smartgwt.client.widgets.tree.TreeGrid;

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
		setAutoSize(true);

		VLayout content = new VLayout();
		content.setWidth100();
		content.setHeight100();
		content.setMembersMargin(3);

		final TreeGrid folders = new FolderTree();
		folders.setWidth100();
		folders.setHeight100();
		folders.setMinHeight(300);

		VLayout buttons = new VLayout();
		buttons.setWidth100();

		Button move = new Button(I18N.message("move"));
		move.setAutoFit(true);
		move.setMargin(1);
		move.addClickHandler(new ClickHandler() {
			public void onClick(ClickEvent event) {
				String shownName = FolderNavigator.get().getSelectedRecord().getAttributeAsString("name");
				long[] selection = FolderNavigator.get().getSelectedIds();
				if (selection.length > 1)
					shownName = selection.length + " " + I18N.message("folders").toLowerCase();

				LD.ask(I18N.message("move"),
						I18N.message("moveask", new String[] { shownName,
								folders.getSelectedRecord().getAttributeAsString("name") }), new BooleanCallback() {

							@Override
							public void execute(Boolean value) {
								if (value) {
									FolderNavigator.get()
											.moveTo(Long.parseLong(folders.getSelectedRecord().getAttributeAsString(
													"folderId")));
								}
								destroy();
							}
						});
			}
		});

		buttons.setMembers(move);

		content.setMembers(folders, buttons);
		addItem(content);
	}
}