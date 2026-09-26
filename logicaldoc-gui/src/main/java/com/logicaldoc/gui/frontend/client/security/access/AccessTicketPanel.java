package com.logicaldoc.gui.frontend.client.security.access;

import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.services.SecurityService;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.UserSelector;
import com.logicaldoc.gui.frontend.client.administration.AdminPanel;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;

/**
 * This popup window is used to get details for the access ticket creation
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3.1
 */
public class AccessTicketPanel extends AdminPanel {

    private static final String MINUTES = "minutes";

    private DynamicForm form;

    public AccessTicketPanel() {
        super("accessticket");

        IButton generate = new IButton(I18N.message("generate"));
        generate.addClickHandler(event -> onGenerate());

        prepareForm();

        body.addMember(form);
        body.addMember(generate);
    }

    private void prepareForm() {
        form = new DynamicForm();
        form.setAlign(Alignment.LEFT);
        form.setNumCols(2);

        SpinnerItem minutesItem = ItemFactory.newSpinnerItem(MINUTES, I18N.message("expiresin"), 30);
        minutesItem.setWrapTitle(false);
        minutesItem.setRequired(true);
        minutesItem.setDefaultValue(30);
        minutesItem.setMin(30);
        minutesItem.setStep(10);
        minutesItem.setHint(I18N.message(MINUTES));

        UserSelector user = new UserSelector("user", "usertoimpersonate", null, false, true);
        user.setValue(1);
        user.setRequired(true);

        StaticTextItem info = ItemFactory.newStaticTextItem("info", I18N.message("accessticketinfo"));
        info.setColSpan(2);
        info.setShowTitle(false);
        info.setTitleOrientation(TitleOrientation.TOP);

        StaticTextItem disclaimer = ItemFactory.newStaticTextItem("disclaimer",
                I18N.message("accessticketdisclaimer"));
        disclaimer.setColSpan(2);
        disclaimer.setShowTitle(false);
        disclaimer.setTitleOrientation(TitleOrientation.TOP);

        form.setItems(info, disclaimer, minutesItem, user);
    }

    public void onGenerate() {
        if (!form.validate())
            return;

        SecurityService.Instance.get().createAccessTicket(Long.parseLong(form.getValueAsString("user")),
                Integer.parseInt(form.getValueAsString(MINUTES)), new DefaultAsyncCallback<>() {

                    @Override
                    public void handleSuccess(List<String> ret) {
                        new AccessTicketDisplay(ret.get(0), ret.get(1)).show();
                    }
                });
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