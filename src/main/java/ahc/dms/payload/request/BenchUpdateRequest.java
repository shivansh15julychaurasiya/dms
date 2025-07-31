package ahc.dms.payload.request;

public class BenchUpdateRequest {
    private Integer id;
    private Integer benchId;

    // Getters and setters
    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Integer getBenchId() {
        return benchId;
    }

    public void setBenchId(Integer benchId) {
        this.benchId = benchId;
    }
}
