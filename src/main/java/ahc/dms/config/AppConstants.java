package ahc.dms.config;

public class AppConstants {

    public static final String PAGE_NUMBER="0";
    public static final String PAGE_SIZE="5";
    public static final String SORT_BY="postId";
    public static final String SORT_DIR="asc";

    public static final Integer ADMIN_USER=1;
    public static final Integer NORMAL_USER=2;

    public static final String JWT_SECRET = "7c2700653ec7ecf51345e95f2f0f2d8322e2cc2147b6f9442e9a5823e0e263eab27bfb8f0e99e83a1b8f19f0";
    public static final long JWT_TOKEN_VALIDITY = 3600000;
    public static final String JWT_CREATED="Created";
    public static final String JWT_REVOKED="Revoked";

    public static final String LOGIN_OTP_URI="http://103.234.185.173/api/swsendnk.asp";
    public static final String LOGIN_OTP_USERNAME="HCALLD";
    public static final String LOGIN_OTP_PASSWORD="44312074";
    public static final String LOGIN_OTP_SENDER="HCALLD";
    public static final String LOGIN_OTP_TEMPLATE_ID="1107168551150460623";
    public static final String LOGIN_OTP_MSG_BEGIN="OTP to login in DMS : ";
    public static final String LOGIN_OTP_MSG_END="- Allahabad High Court";

}
