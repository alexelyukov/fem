import { Injectable } from '@angular/core';
import { HttpClient, HttpHeaders } from '@angular/common/http';

@Injectable()
export class ApiService {
    private url = 'http://localhost:3000';

    constructor(private http: HttpClient) { }

    getThermalFieldRectangleTriangulation(geometry) {
        return this.http.post(`${this.url}/thermal-field-rectangle-triangulation`, `geometry=${geometry}`);
    }

    
}