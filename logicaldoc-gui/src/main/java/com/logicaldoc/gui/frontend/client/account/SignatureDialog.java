package com.logicaldoc.gui.frontend.client.account;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIStamp;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.frontend.client.metadata.stamp.StampUploader;
import com.logicaldoc.gui.frontend.client.services.StampService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.toolbar.ToolStrip;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;

/**
 * This is the form used to display and generate the user's signature stamp
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.5
 */
public class SignatureDialog extends Window {

	public SignatureDialog() {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("signature"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		setHeight(390);
		setWidth(610);

		centerInPage();

		StampService.Instance.get().getSignature(new AsyncCallback<>() {

			@Override
			public void onFailure(Throwable caught) {
				GuiLog.serverError(caught);
			}

			@Override
			public void onSuccess(GUIStamp stamp) {
				SignaturePanel signaturePanel = new SignaturePanel(stamp);

				ToolStripButton save = new ToolStripButton(I18N.message("save"));
				save.addClickHandler(click -> {
					if (signaturePanel.validate())
						StampService.Instance.get().save(signaturePanel.getStamp(), new AsyncCallback<>() {

							@Override
							public void onFailure(Throwable caught) {
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(GUIStamp arg) {
								destroy();
							}
						});
				});

				ToolStripButton uploadSignaure = new ToolStripButton(I18N.message("uploadyoursignature"));
				uploadSignaure.addClickHandler(click -> new StampUploader(stamp.getId(), signaturePanel).show());

				ToolStripButton close = new ToolStripButton(I18N.message("close"));
				close.addClickHandler(click -> destroy());

				ToolStrip toolStrip = new ToolStrip();
				toolStrip.setWidth100();
				toolStrip.addButton(uploadSignaure);
				toolStrip.addButton(save);
				toolStrip.addFill();
				toolStrip.addButton(close);

				addItem(toolStrip);
				addItem(signaturePanel);
			}
		});
	}
}