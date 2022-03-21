import * as React from "react";
import { Users } from "Api/Users";

export const useOrganizersPagination = () => {
  const [organizers, setOrganizers] = React.useState<any[]>([]);
  const [organizersPage, setOrganizersPage] = React.useState<number>(1);
  const [organizersLimit, setOrganizersLimit] = React.useState<number>(10);

  React.useEffect(() => {
    (async () => getOrganizersPaginated())();
  }, []);

  const getOrganizersPaginated = async (page?: number) => {
    try {
      const res = await Users.getAllOrganizers({
        limit: organizersLimit,
        page: page ?? organizersPage,
      });

      if (res) {
        setOrganizers(res.result);
        setOrganizersPage((prev) => prev + 1);
      }
    } catch (e) {
      console.error(e);
    }
  };

  return {
    organizers,
    organizersPage,
    setOrganizersPage,
    organizersLimit,
    setOrganizersLimit,
    getOrganizersPaginated,
  };
};
