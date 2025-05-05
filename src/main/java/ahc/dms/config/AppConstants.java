package ahc.dms.config;

import java.util.Set;

public class AppConstants {

    public static final String[] PUBLIC_URLS = {
            "/dms/auth/login-password",
            "/dms/auth/login-otp",
            "/dms/auth/register",
            "/dms/auth/request-otp",
            "/dms/auth/verify-reset-otp",
            "/actuator/**"
    };

    public static final String[] WEB_IGNORES = {
            "/swagger-ui/**",
            "/v3/api-docs*/**",
            "/resources/static/**"
    };


    /*
        Ant-style wildcards:
            ? - matches one character
            * - matches zero or more characters within a path segment
            ** - matches zero or more path segments
            {string} - matches a path segment and captures it as a variable
     */
    public static final Set<String> JWT_IGNORED_URLS = Set.of(
            "/dms/auth/login-password",
            "/dms/auth/login-otp",
            "/dms/auth/register",
            "/dms/auth/request-otp",
            "/dms/auth/verify-reset-otp",
            "/actuator/**"
    );

    public static final Set<String> REQUEST_AUTH_IGNORED_URLS = Set.of(
            "/dms/auth/login-password",
            "/dms/auth/login-otp",
            "/dms/auth/register",
            "/dms/auth/request-otp",
            "/dms/auth/verify-reset-otp",
            "/actuator/**"
    );

    public static final String PAGE_NUMBER = "0";
    public static final String PAGE_SIZE = "5";
    public static final String SORT_BY = "roleId";
    public static final String SORT_DIR = "asc";

    public static final Integer ADMIN_USER = 1;
    public static final Integer NORMAL_USER = 2;
    public static final Integer ECOURT_USER = 3;

    public static final String JWT_SECRET = "7c2700653ec7ecf51345e95f2f0f2d8322e2cc2147b6f9442e9a5823e0e263eab27bfb8f0e99e83a1b8f19f0";
    public static final long JWT_TOKEN_VALIDITY = 36000000;
    public static final String JWT_CREATED = "Created";
    public static final String JWT_REVOKED = "Revoked";

    public static final String OTP_TYPE_LOGIN = "Login";
    public static final String OTP_TYPE_RESET = "Reset";

    public static final String LOGIN_OTP_URI = "http://103.234.185.173/api/swsendnk.asp";
    public static final String LOGIN_OTP_USERNAME = "HCALLD";
    public static final String LOGIN_OTP_PASSWORD = "44312074";
    public static final String LOGIN_OTP_SENDER = "HCALLD";
    public static final String LOGIN_OTP_TEMPLATE_ID = "1107168551150460623";
    public static final String LOGIN_OTP_MSG_BEGIN = "OTP to login in DMS : ";
    public static final String LOGIN_OTP_MSG_END = " - Allahabad High Court";

}
