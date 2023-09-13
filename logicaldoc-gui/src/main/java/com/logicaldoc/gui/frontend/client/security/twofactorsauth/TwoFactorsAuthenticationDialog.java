package com.logicaldoc.gui.frontend.client.security.twofactorsauth;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.controllers.UserController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.LD;
import com.logicaldoc.gui.frontend.client.services.TwoFactorsAuthenticationService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows the popup to change the 2fa of a user.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class TwoFactorsAuthenticationDialog extends Window {

	public TwoFactorsAuthenticationDialog(final GUIUser user, boolean allowNotify) {
		super();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("twofactorsauth"));
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		final TwoFactorsAuthenticationPanel panel = new TwoFactorsAuthenticationPanel(user, allowNotify);
		panel.setWidth(350);

		IButton save = new IButton(I18N.message("save"));
		save.addClickHandler(event -> {
			if (panel.validate()) {
				LD.contactingServer();
				save.setDisabled(true);
				TwoFactorsAuthenticationService.Instance.get().changeTwoFactorsAuthentication(user.getId(),
						panel.getFactor(), panel.getKey(), panel.getAccount(), panel.isNotify(),
						new AsyncCallback<Void>() {
							@Override
							public void onFailure(Throwable caught) {
								LD.clearPrompt();
								save.setDisabled(false);
								GuiLog.serverError(caught);
							}

							@Override
							public void onSuccess(Void arg) {
								LD.clearPrompt();
								if (panel != null) {
									user.setSecondFactor(panel.getFactor());
									user.setKey(panel.getKey());
								}
								UserController.get().changed(user);
								TwoFactorsAuthenticationDialog.this.destroy();
							}
						});
			}
		});

		VLayout body = new VLayout();
		body.setMembers(panel, save);
		addItem(body);
	}
}