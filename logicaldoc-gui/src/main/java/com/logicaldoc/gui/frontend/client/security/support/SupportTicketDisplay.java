package com.logicaldoc.gui.frontend.client.security.support;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.common.client.widgets.DelayedRedrawWindow;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.RowSpacerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Displays a created support ticket details
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.3.1
 */
public class SupportTicketDisplay extends DelayedRedrawWindow {

    public SupportTicketDisplay(String ticketId, String password) {
        setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
        setTitle(I18N.message("supportticket"));
        setCanDragResize(true);
        setIsModal(true);
        setShowModalMask(true);
        centerInPage();
        setAutoSize(true);

        DynamicForm form = new DynamicForm();
        form.setMargin(4);
        form.setTitleOrientation(TitleOrientation.LEFT);

        StaticTextItem ticketIdItem = ItemFactory.newStaticTextItem("ticketid", ticketId);
        ticketIdItem.setWrap(false);
        ticketIdItem.setWrapTitle(false);

        StaticTextItem passwordItem = ItemFactory.newStaticTextItem("password", password);
        passwordItem.setWrap(false);
        passwordItem.setWrapTitle(false);

        StaticTextItem advice = ItemFactory.newStaticTextItem("advice", I18N.message("supportticketdisplay", ticketId));
        advice.setColSpan(2);
        advice.setShowTitle(false);
        advice.setTitleOrientation(TitleOrientation.TOP);

        StaticTextItem disclaimer = ItemFactory.newStaticTextItem("disclaimer", I18N.message("supportticketdisclaimer"));
        disclaimer.setColSpan(2);
        disclaimer.setShowTitle(false);
        disclaimer.setTitleOrientation(TitleOrientation.TOP);
        
        form.setItems(advice, new RowSpacerItem(), ticketIdItem, passwordItem, new RowSpacerItem(), disclaimer);

        IButton close = new IButton(I18N.message("close"));
        close.addClickHandler(event -> SupportTicketDisplay.this.destroy());

        IButton copy = new IButton(I18N.message("copy"));
        copy.addClickHandler(event -> Util.copyText("Support Ticket ID: " + ticketId + "\nPassword: " + password));

        HLayout buttons = new HLayout();
        buttons.setMembersMargin(2);
        buttons.setMembers(close, copy);

        VLayout layout = new VLayout();
        layout.setMembersMargin(5);
        layout.addMember(form);
        layout.addMember(buttons);

        addItem(layout);
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