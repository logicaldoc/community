package com.logicaldoc.gui.frontend.client.security.twofactorsauth;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.beans.GUIUser;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.TwoFactorsAuthenticationService;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Label;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.KeyPressEvent;
import com.smartgwt.client.widgets.form.fields.events.KeyPressHandler;

/**
 * Panel for setting up the YubiKey second factor.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7.3
 */
public class YubiKeySetup extends TwoFactorsAuthenticationSetup {

	private GUIUser user;

	private DynamicForm form;

	private Label hint;

	public YubiKeySetup(GUIUser user) {
		super();
		setWidth100();
		setHeight100();
		setMembersMargin(5);
		setMargin(5);

		this.user = user;

		// Prepare the form with account informations
		TextItem keyItem = ItemFactory.newTextItem("key", I18N.message("key"), null);
		keyItem.setWidth(300);
		keyItem.addKeyPressHandler(new KeyPressHandler() {
			@Override
			public void onKeyPress(KeyPressEvent event) {
				if (event.getKeyName() != null && "enter".equals(event.getKeyName().toLowerCase())) {
					onSubmitKey();
				}
			}
		});

		form = new DynamicForm();
		form.setTitleOrientation(TitleOrientation.LEFT);
		form.setNumCols(4);
		form.setItems(keyItem);

		hint = new Label(I18N.message(Constants.TWOFA_YUBIKEY + ".hint1"));

		setMembers(hint, form);
	}

	private void onSubmitKey() {
		TwoFactorsAuthenticationService.Instance.get().generateYubiKeyCredentials(form.getValueAsString("key"),
				new AsyncCallback<String>() {
					@Override
					public void onFailure(Throwable caught) {
						GuiLog.serverError(caught);
					}

					@Override
					public void onSuccess(String publicId) {
						key = publicId;
						user.setKey(publicId);
						removeMember(form);
						hint.setContents(I18N.message(Constants.TWOFA_YUBIKEY + ".hint2"));
					}
				});
	}
}