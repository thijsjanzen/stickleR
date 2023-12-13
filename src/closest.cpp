
//  Created by Thijs Janzen on 11/04/2019.
//

#include <Rcpp.h>
#include <vector>

double find_site(const double& antenna,
                 const Rcpp::NumericMatrix& site_map) {

  double out = NA_REAL;

  for (int i = 0; i < site_map.nrow(); ++i) {
    if (site_map(i, 0) == antenna) {
      out = site_map(i, 1);
      break;
    }
  }
  return out;
}

//' function to get all locations
//' @param max_sec maxsec
//' @param times tt
//' @param antennae aa
//' @param site_map sm
//' @return vector of sites
//' @export
// [[Rcpp::export]]
std::vector<double> get_all_locations_cpp(double start_time,
                                          double end_time,
                                          const std::vector<double>& times,
                                          const std::vector<double>& antennae,
                                          const Rcpp::NumericMatrix& site_map,
                                          double prev_day_location) {

  std::vector<double> sites(antennae.size(), 0);
  for (size_t i = 0; i < antennae.size(); ++i) {
    sites[i] = find_site(antennae[i], site_map);
  }


  double total_time_steps = end_time - start_time + 1;
  std::vector<double> out;
  out.reserve(total_time_steps);
  out.push_back(prev_day_location); //out[0] = prev_day_location;


  for (size_t i = start_time + 1; i <= end_time; ++i) {
      auto first_equal_or_greater = std::lower_bound(times.begin(), times.end(), i);

      if (first_equal_or_greater == times.end()) {
        // here we lack data past the end of the recording, but we can extrapolate from the last known location
        out.push_back(sites.back());
      } else {
        if (*first_equal_or_greater > i) first_equal_or_greater--;

        auto index = std::distance(times.begin(), first_equal_or_greater);

        if (times[index] < start_time) {
          // in case we recover a time point before the start of the experiment, we record the known starting location
          out.push_back(prev_day_location);
        } else {
          out.push_back(sites[index]);
        }
      }
  }

  return out;
}
